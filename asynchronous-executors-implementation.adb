with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Asynchronous.Tasks;
with Events.Composition.Shortcuts;
with Events.Interfaces;
with Events.Servers;
with Utilities.Debug;
with Utilities.References;
with Utilities.References.Nullable;
with Utilities.Signals;
pragma Elaborate_All (Utilities.References.Nullable);

package body Asynchronous.Executors.Implementation is

   -- Let us define some convenience notations first
   package Async_Tasks renames Asynchronous.Tasks;
   use all type Events.Interfaces.Event_Status;

   -- === TASK INSTANCES ===

   -- An executor manipulates task instances, which are composed of a task object and some associated metadata
   type Task_Access is access Interfaces.Any_Async_Task;
   type Task_Instance is new Ada.Finalization.Limited_Controlled with
      record
         Task_Object : Task_Access := null;
         Completion_Event : Events.Servers.Server;
      end record;
   pragma Preelaborable_Initialization (Task_Instance);

   -- Because instances must contain pointers, we should make sure that they are always finalized properly
   overriding procedure Finalize (Who : in out Task_Instance) is
      procedure Liberate_Task is new Ada.Unchecked_Deallocation (Interfaces.Any_Async_Task, Task_Access);
   begin
      Liberate_Task (Who.Task_Object);
   end Finalize;

   -- To be able to move task instances around, we need some kind of reference to them. In C++ terms, what we would
   -- really like is a unique_ptr, as we only need move semantics. But a shared reference, although somewhat
   -- inefficient from a performance point of view, will do fine as a first implementation.
   package Task_Instance_Reference_Base is new Utilities.References (Task_Instance);
   package Task_Instance_References is new Task_Instance_Reference_Base.Nullable;
   subtype Task_Instance_Reference is Task_Instance_References.Reference;

   -- There should be a convenient way to make a reference-counted task instance from a task object
   function Make_Task_Instance (From : Interfaces.Any_Async_Task) return Task_Instance_Reference is
   begin
      return Ref : constant Task_Instance_Reference := Task_Instance_References.Make do
         Ref.Get.Task_Object := new Interfaces.Any_Async_Task'(From);
      end return;
   end Make_Task_Instance;

   -- === TASK QUEUES ===

   -- Ready asynchronous tasks will be put on a FIFO queue, and executed once worker threads become available.
   package Task_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Task_Instance_Reference);
   package Task_Queues is new Ada.Containers.Unbounded_Synchronized_Queues (Task_Queue_Interfaces);

   -- Because task queues must be at global scope in order to allow for asynchronous queueing, and must be
   -- shared across multiple tasks, reference counting must be used here as well.
   package Task_Queue_Reference_Base is new Utilities.References (Task_Queues.Queue);
   package Task_Queue_References is new Task_Queue_Reference_Base.Nullable;
   subtype Task_Queue_Reference is Task_Queue_References.Reference;
   function Make_Task_Queue return Task_Queue_Reference renames Task_Queue_References.Make;

   -- === TASK SCHEDULING ===

   -- When a task has an event wait list, we want to compute the AND-conjunction of that wait list, wait for it to go
   -- anywhere, and react according to the final event status. This is what the following code is about.
   procedure Schedule_Pending_Task (Who   : Task_Instance_Reference;
                                    After : Async_Tasks.Event_Wait_List;
                                    On    : in out Task_Queue_Reference);

   -- The rest is just the underlying machinery that makes Schedule_Pending_Task work
   Wait_List_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

   procedure Schedule_Ready_Task (Who          : Task_Instance_Reference;
                                  According_To : Events.Interfaces.Finished_Event_Status;
                                  On           : in out Task_Queue_Reference) is
   begin
      case According_To is
         when Done =>
            On.Get.Enqueue (Who);
         when Canceled =>
            Who.Get.Completion_Event.Cancel;
         when Error =>
            Who.Get.Completion_Event.Mark_Error (Wait_List_Error_Occurence);
      end case;
   end Schedule_Ready_Task;

   type Scheduled_Task is new Events.Interfaces.Event_Listener_Reference with
      record
         Instance : Task_Instance_Reference;
         Target_Queue : Task_Queue_Reference;
      end record;

   overriding procedure Notify_Event_Status_Change (Where : in out Scheduled_Task;
                                                    What  : Events.Interfaces.Finished_Event_Status) is
   begin
      Schedule_Ready_Task (Who          => Where.Instance,
                           According_To => What,
                           On           => Where.Target_Queue);
   end Notify_Event_Status_Change;

   procedure Schedule_Pending_Task (Who   : Task_Instance_Reference;
                                    After : Async_Tasks.Event_Wait_List;
                                    On    : in out Task_Queue_Reference) is
      Input_Event : Interfaces.Event_Client := Events.Composition.Shortcuts.When_All (After);
   begin
      case Input_Event.Status is
         when Events.Interfaces.Finished_Event_Status =>
            Schedule_Ready_Task (Who          => Who,
                                 According_To => Input_Event.Status,
                                 On           => On);
         when Pending =>
            declare
               Scheduled : Scheduled_Task := (Instance => Who, Target_Queue => On);
            begin
               Input_Event.Add_Listener (Scheduled);
            end;
      end case;
   end Schedule_Pending_Task;

   -- === EXECUTOR TASK IMPLEMENTATION ===

   -- Let us also define two scheduling policies:
   --    - In batch mode, executors run tasks as long as they can, which maximizes computation performance.
   --    - In round-robin mode, executors switch between tasks in a cyclic FIFO fashion, which minimizes starvation.
   type Scheduling_Policy is (Batch, Round_Robin);
   Active_Scheduling_Policy : constant Scheduling_Policy := Round_Robin;

   -- Now we can define task executors
   task body Executor_Task is

      -- This is the FIFO queue that should be used by this executor
      Ready_Tasks : Task_Queue_Reference := Make_Task_Queue;

      -- Because worker threads interact with protected objects, terminate alternatives cannot be used. Instead, we go
      -- for a manual termination procedure, which is requested from workers using a signal object.
      Stop_Request : Utilities.Signals.Signal;

      -- Ready tasks are executed by workers, which are defined as follows
      task type Worker;
      task body Worker is

         -- The currently processed task will end up here
         Work_Item : Task_Instance_Reference;

         -- This function runs incoming tasks and tells whether they are yielding or not
         function Run_Work_Item return Boolean is
         begin
            declare
               use all type Async_Tasks.Return_Status;
               Work_Item_Output : constant Async_Tasks.Return_Value := Work_Item.Get.Task_Object.Run;
               Work_Item_Yielding : Boolean := False;
            begin
               case Async_Tasks.Status (Work_Item_Output) is
                  when Finished =>
                     Work_Item.Get.Completion_Event.Mark_Done;
                  when Yielding =>
                     Work_Item_Yielding := True;
                  when Waiting =>
                     Schedule_Pending_Task (Who   => Work_Item,
                                            After => Async_Tasks.Wait_List (Work_Item_Output),
                                            On    => Ready_Tasks);
               end case;
               return Work_Item_Yielding;
            end;
         exception
            when E : others =>
               Work_Item.Get.Completion_Event.Mark_Error (E);
               return False;
         end Run_Work_Item;

         -- This function processes work items according to the current scheduling policy
         procedure Process_Work_Item is
         begin
            case Active_Scheduling_Policy is
               when Round_Robin =>
                  if Run_Work_Item then
                     Ready_Tasks.Get.Enqueue (Work_Item);
                  end if;
               when Batch =>
                  while Run_Work_Item loop
                     null;
                  end loop;
            end case;
         end Process_Work_Item;

         -- Work will continue as long as this flag is active
         Worker_Active : Boolean := True;

      begin
         while Worker_Active loop
            select
               Ready_Tasks.Get.Dequeue (Work_Item);
               Process_Work_Item;
            then abort
               Stop_Request.Wait;
               Worker_Active := False;
            end select;
         end loop;
      exception
         when E : others =>
            Utilities.Debug.Display_Unhandled_Exception ("an asynchronous worker", E);
            Stop_Request.Send;
            raise;
      end Worker;

      -- We define the following flock of workers
      type Worker_Array is array (Interfaces.Worker_Count range <>) of Worker;
      Workers : Worker_Array (1 .. Number_Of_Workers) with Unreferenced;

      -- Finally, executor activity is controlled by a shared flag
      Executor_Active : Boolean := True;

   begin
      while Executor_Active loop
         select
            accept Schedule_Task (What  : Interfaces.Any_Async_Task;
                                  After : Interfaces.Event_Wait_List;
                                  Event : out Interfaces.Event_Client) do
               declare
                  Work_Item : constant Task_Instance_Reference := Make_Task_Instance (What);
               begin
                  Event := Work_Item.Get.Completion_Event.Make_Client;
                  Schedule_Pending_Task (Who   => Work_Item,
                                         After => After,
                                         On    => Ready_Tasks);
               end;
            end Schedule_Task;
         or
            accept Stop do
               Stop_Request.Send;
               Executor_Active := False;
            end Stop;
         or
            terminate;
         end select;
      end loop;
   exception
      when E : others =>
         Utilities.Debug.Display_Unhandled_Exception ("an asynchronous executor", E);
         Stop_Request.Send;
         raise;
   end Executor_Task;

begin

   -- Save an occurence of Error_In_Wait_List, to be propagated as needed
   begin
      raise Interfaces.Error_In_Wait_List;
   exception
      when E : Interfaces.Error_In_Wait_List =>
         Ada.Exceptions.Save_Occurrence (Target => Wait_List_Error_Occurence,
                                         Source => E);
   end;

end Asynchronous.Executors.Implementation;
