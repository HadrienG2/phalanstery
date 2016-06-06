with Asynchronous.Events.Interfaces;
with Asynchronous.Events.Servers;
with Asynchronous.Executors.Scheduling;
with Asynchronous.Executors.Task_Instances.References;
with Asynchronous.Executors.Task_Queues;
with Asynchronous.Tasks;
with Asynchronous.Utilities.Barriers;
with Asynchronous.Utilities.Debug;
with Asynchronous.Utilities.Signals;

package body Asynchronous.Executors.Implementation is

   -- Let us define some convenience notations first
   subtype Task_Instance_Reference is Task_Instances.References.Reference;
   subtype Task_Queue_Reference is Task_Queues.Reference;
   use all type Events.Interfaces.Event_Status;

   -- Let us also define two executor scheduling policies:
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
                     Scheduling.Schedule_Task (Who   => What,
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
                  Scheduling.Schedule_Task (Who   => Work_Item,
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

end Asynchronous.Executors.Implementation;
