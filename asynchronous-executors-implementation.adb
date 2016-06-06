with Ada.Exceptions;
with Asynchronous.Events.Composition.Shortcuts;
with Asynchronous.Events.Interfaces;
with Asynchronous.Events.Servers;
with Asynchronous.Executors.Task_Instances.References;
with Asynchronous.Executors.Task_Queues;
with Asynchronous.Tasks;
with Asynchronous.Utilities.Barriers;
with Asynchronous.Utilities.Debug;
with Asynchronous.Utilities.Exceptions;
with Asynchronous.Utilities.Signals;
pragma Elaborate_All (Asynchronous.Utilities.Exceptions);

package body Asynchronous.Executors.Implementation is

   -- Let us define some convenience notations first
   subtype Finished_Event_Status is Events.Interfaces.Finished_Event_Status;
   subtype Task_Instance_Reference is Task_Instances.References.Reference;
   subtype Task_Queue_Reference is Task_Queues.Reference;
   use all type Events.Interfaces.Event_Status;

   -- === TASK SCHEDULING ===

   -- When a task has an event wait list, we want to compute the AND-conjunction of that wait list, wait for it to go
   -- anywhere, and react according to the final event status. This is what the following code is about.
   procedure Schedule_Pending_Task (Who   : Task_Instance_Reference;
                                    After : Interfaces.Event_Wait_List;
                                    On    : Task_Queue_Reference);

   -- The rest is just the underlying machinery that makes Schedule_Pending_Task work
   Wait_List_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

   procedure Schedule_Ready_Task (Who          : Task_Instance_Reference;
                                  According_To : Finished_Event_Status;
                                  On           : Task_Queue_Reference) is
   begin
      case According_To is
         when Done =>
            On.Set.Enqueue (Who);
         when Canceled =>
            Who.Set.Completion_Event.Cancel;
         when Error =>
            Who.Set.Completion_Event.Mark_Error (Wait_List_Error_Occurence);
      end case;
   end Schedule_Ready_Task;

   type Scheduled_Task is new Events.Interfaces.Event_Listener_Reference with
      record
         Instance : Task_Instance_Reference;
         Target_Queue : Task_Queue_Reference;
      end record;

   overriding procedure Notify_Event_Status_Change (Where : in out Scheduled_Task;
                                                    What  : Finished_Event_Status) is
   begin
      Schedule_Ready_Task (Who          => Where.Instance,
                           According_To => What,
                           On           => Where.Target_Queue);
   end Notify_Event_Status_Change;

   procedure Schedule_Pending_Task (Who   : Task_Instance_Reference;
                                    After : Interfaces.Event_Wait_List;
                                    On    : Task_Queue_Reference) is
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
   Active_Scheduling_Policy : constant Scheduling_Policy := Batch;

   -- Now we can define task executors
   task body Executor_Task is

      -- This is the FIFO queue that should be used by this executor
      Ready_Tasks : constant Task_Queue_Reference := Task_Queues.Make_Task_Queue;

      -- Because worker threads interact with protected objects, terminate alternatives cannot be used. Instead, we go
      -- for a manual termination procedure, which is requested from workers using a signal object, and acknowledged
      -- using a barrier object.
      Stop_Request : Utilities.Signals.Signal;
      Stop_Barrier : Utilities.Barriers.Barrier (Natural (Number_Of_Workers));

      -- Ready tasks are executed by workers, which are defined as follows
      task type Worker;
      task body Worker is

         -- This function runs incoming tasks and tells whether they are yielding or not
         function Run_Work_Item (What : Task_Instance_Reference) return Boolean is
         begin
            declare
               use all type Tasks.Return_Status;
               Work_Item_Output : constant Tasks.Return_Value := What.Get.Task_Object.Run;
               Work_Item_Yielding : Boolean := False;
            begin
               case Tasks.Status (Work_Item_Output) is
                  when Finished =>
                     What.Set.Completion_Event.Mark_Done;
                  when Yielding =>
                     Work_Item_Yielding := True;
                  when Waiting =>
                     Schedule_Pending_Task (Who   => What,
                                            After => Tasks.Wait_List (Work_Item_Output),
                                            On    => Ready_Tasks);
               end case;
               return Work_Item_Yielding;
            end;
         exception
            when E : others =>
               What.Set.Completion_Event.Mark_Error (E);
               return False;
         end Run_Work_Item;

         -- This function processes work items according to the current scheduling policy
         procedure Process_Work_Item (What : Task_Instance_Reference) is
         begin
            case Active_Scheduling_Policy is
               when Round_Robin =>
                  if Run_Work_Item (What) then
                     Ready_Tasks.Set.Enqueue (What);
                  end if;
               when Batch =>
                  while Run_Work_Item (What) loop
                     null;
                  end loop;
            end case;
         end Process_Work_Item;

         -- Work will continue as long as this flag is active
         Worker_Active : Boolean := True;

      begin
         while Worker_Active loop
            declare
               Work_Item : Task_Instance_Reference;
            begin
               -- Operate in an even-driven fashion during normal operation
               select
                  Ready_Tasks.Set.Dequeue (Work_Item);
                  Process_Work_Item (Work_Item);
               then abort
                  Stop_Request.Wait;
                  Worker_Active := False;
               end select;
            end;
         end loop;
         Stop_Barrier.Join;
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
                  Work_Item : constant Task_Instance_Reference := Task_Instances.References.Make_Task_Instance (What);
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
               Stop_Barrier.Wait;
            end Stop;
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
   Utilities.Exceptions.Make_Occurrence (What  => Interfaces.Error_In_Wait_List'Identity,
                                         Where => Wait_List_Error_Occurence);

end Asynchronous.Executors.Implementation;
