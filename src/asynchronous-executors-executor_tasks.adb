with Asynchronous.Events.Clients;
with Asynchronous.Events.Interfaces;
with Asynchronous.Events.Servers;
with Asynchronous.Executors.Scheduling;
with Asynchronous.Executors.Task_Instances.References;
with Asynchronous.Executors.Task_Queues.References;
with Asynchronous.Tasks;
with Asynchronous.Tasks.Trivial;
with Asynchronous.Utilities.Barriers;
with Asynchronous.Utilities.Debug;
with Asynchronous.Utilities.Signals;
with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Executors.Executor_Tasks is

   subtype Valid_Task_Instance_Reference is Task_Instances.References.Valid_Reference;
   subtype Valid_Task_Queue_Reference is Task_Queues.References.Valid_Reference;
   use all type Events.Interfaces.Event_Status;

   task body Executor_Task is

      -- Executor tasks hold ready tasks (aka work items) on a FIFO queue
      Task_Queue : constant Valid_Task_Queue_Reference := Task_Queues.References.Make_Task_Queue;

      -- Because worker threads are commanded using protected objects rather than task entries, terminate alternatives
      -- cannot be used. Instead, we go through a manual termination procedure: the executor task requests worker
      -- termination using a signal object, and acknowledges it using a barrier object.
      Stop_Request : Utilities.Signals.Signal;
      Stop_Barrier : Utilities.Barriers.Barrier (Natural (Number_Of_Workers));

      -- Work items are executed by a flock of worker threads, which are defined as follows
      task type Worker;
      task body Worker is

         -- This function runs a work item and tells whether it is yielding or not
         function Run_Work_Item (What : Valid_Task_Instance_Reference) return Boolean is
            use all type Tasks.Return_Status;
            Work_Item_Yielding : Boolean := False;
         begin
            declare
               Work_Item_Output : constant Tasks.Return_Value := What.Get.Task_Object.Run;
            begin
               case Tasks.Status (Work_Item_Output) is
                  when Finished =>
                     What.Set.Completion_Event.Mark_Done;
                  when Yielding =>
                     Work_Item_Yielding := True;
                  when Waiting =>
                     Scheduling.Schedule_Task (Who   => What,
                                               After => Tasks.Wait_List (Work_Item_Output),
                                               On    => Task_Queue);
               end case;
               return Work_Item_Yielding;
            end;
         exception
            when E : others =>
               What.Set.Completion_Event.Mark_Error (E);
               return False;
         end Run_Work_Item;

         -- This function processes a work item according to the current scheduling policy
         procedure Process_Work_Item (What : Valid_Task_Instance_Reference) is
         begin
            case Active_Scheduling_Policy is
               when Round_Robin =>
                  if Run_Work_Item (What) then
                     Task_Queue.Set.Ready.Enqueue (What);
                  end if;
               when Batch =>
                  loop
                     exit when not Run_Work_Item (What);
                  end loop;
            end case;
         end Process_Work_Item;

         -- Workers will wait for work as long as this flag is active
         Worker_Active : Boolean := True;

      begin

         -- Process ready tasks, and wait for them unless the stop signal is set
         while Worker_Active loop
            declare
               Work_Item : Task_Instances.References.Reference;
            begin
               select
                  Task_Queue.Set.Ready.Dequeue (Work_Item);
                  Process_Work_Item (Work_Item);
               then abort
                  Stop_Request.Wait;
                  Worker_Active := False;
               end select;
            end;
         end loop;

         -- Signal the underlying executor task that all work is complete
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

      -- Executor tasks will wait for work until this flag goes to False
      Executor_Active : Boolean := True;

   begin
      while Executor_Active loop
         select
            accept Schedule_Task (What  : Interfaces.Any_Async_Task;
                                  After : Interfaces.Event_Wait_List;
                                  Event : out Interfaces.Valid_Event_Client) do
               declare
                  Work_Item : constant Valid_Task_Instance_Reference :=
                    Task_Instances.References.Make_Task_Instance (What);
               begin
                  Event := Work_Item.Get.Completion_Event.Make_Client;
                  Scheduling.Schedule_Task (Who   => Work_Item,
                                            After => After,
                                            On    => Task_Queue);
               end;
            end Schedule_Task;
         or
            accept Stop do
               Task_Queue.Set.Pending.Flush_Pending;
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


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      subtype Event_Client is Events.Clients.Client;

      Number_Of_Workers : constant := 2;
      Empty_Wait_List : Interfaces.Event_Wait_List (2 .. 1);

      procedure Test_Null_Task is
         Executor : Executor_Task (Number_Of_Workers);
         T : Tasks.Trivial.Null_Task;
         Client : Event_Client;
      begin
         select
            Executor.Schedule_Task (What  => T,
                                    After => Empty_Wait_List,
                                    Event => Client);
         or
            delay 0.02;
            Fail ("An executor should accept tasks quickly");
         end select;
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("The null task should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Done),
                       Message => "The null task should appear completed after its executor has terminated");
      end Test_Null_Task;

      procedure Test_Yielding_Task is
         Executor : Executor_Task (Number_Of_Workers);
         T : Tasks.Trivial.Yielding_Task (1);
         Client : Event_Client;
      begin
         Executor.Schedule_Task (What  => T,
                                 After => Empty_Wait_List,
                                 Event => Client);
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("A simple yielding task should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Done),
                       Message => "The yielding task should appear completed after its executor has terminated");
      end Test_Yielding_Task;

      procedure Test_Erronerous_Task is
         Executor : Executor_Task (Number_Of_Workers);
         T : Tasks.Trivial.Erronerous_Task;
         Client : Event_Client;
      begin
         Executor.Schedule_Task (What  => T,
                                 After => Empty_Wait_List,
                                 Event => Client);
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("The erronerous task should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Error),
                       Message => "The erronerous task should go to the Error state after its executor has terminated");
      end Test_Erronerous_Task;

      procedure Test_Canceled_Wait_Task is
         Executor : Executor_Task (Number_Of_Workers);
         T : Tasks.Trivial.Canceled_Wait_Task;
         Client : Event_Client;
      begin
         Executor.Schedule_Task (What  => T,
                                 After => Empty_Wait_List,
                                 Event => Client);
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("A task waiting for a canceled event should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Canceled),
                       Message => "The canceled-wait task should appear canceled after its executor has terminated");
      end Test_Canceled_Wait_Task;

   begin
      Test_Null_Task;
      Test_Yielding_Task;
      Test_Erronerous_Task;
      Test_Canceled_Wait_Task;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Asynchronous.Executors.Executor_Tasks;
