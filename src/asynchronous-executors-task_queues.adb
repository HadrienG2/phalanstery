with Asynchronous.Utilities.Debug;
with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Executors.Task_Queues is

   use type Ada.Containers.Count_Type;

   procedure Add_Task (Where : in out Pending_Counter) is
   begin
      Atomic_Counters.Increment (Where.Implementation);
   end Add_Task;

   procedure Remove_Task (Where : in out Pending_Counter) is
      Negative_Pending_Count : constant Boolean := Atomic_Counters.Decrement (Where.Implementation);
   begin
      pragma Assert (not Negative_Pending_Count, "The amount of pending tasks should never become negative!");
   end Remove_Task;

   function No_Pending_Task (Where : Pending_Counter) return Boolean is
      (Atomic_Counters.Is_One (Where.Implementation));

   procedure Flush (What : Task_Queue) is
   begin
      while not What.Is_Empty loop
         delay 0.05;
      end loop;
   end Flush;

   not overriding function Is_Empty (What : Task_Queue) return Boolean is
     ((What.Ready.Current_Use = 0) and (What.Pending.No_Pending_Task));

   overriding procedure Finalize (What : in out Task_Queue) is
   begin
      if not What.Is_Empty then
         Utilities.Debug.Display ("A task queue was discarded as it still had tasks in it!");
         raise Queue_Usage_Error;
      end if;
   end Finalize;

   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Pending_Counter is
         C : Pending_Counter;
      begin

         Assert_Truth (Check   => C.No_Pending_Task,
                       Message => "Pending counters should initially feature no pending task");

         C.Add_Task;
         Assert_Truth (Check   => (not C.No_Pending_Task),
                       Message => "After adding a pending task, counters should not signal it");

         C.Remove_Task;
         Assert_Truth (Check   => C.No_Pending_Task,
                       Message => "After deleting the pending tasks, counters, should go back to the empty state");

      end Test_Pending_Counter;

      procedure Test_Task_Queue is
         TI : Task_Instance_Reference;
         TQ : Task_Queue;
      begin

         Assert_Truth (Check   => TQ.Is_Empty,
                       Message => "Task queues should be born empty");

         TQ.Pending.Add_Task;
         Assert_Truth (Check   => (not TQ.Is_Empty),
                       Message => "Task queues with a pending task should not be empty");

         TQ.Ready.Enqueue (TI);
         Assert_Truth (Check   => (not TQ.Is_Empty),
                       Message => "Task queues with a pending task and a queued task should not be empty");

         TQ.Pending.Remove_Task;
         Assert_Truth (Check   => (not TQ.Is_Empty),
                       Message => "Task queues with a queued task should not be empty");

         TQ.Ready.Dequeue (TI);
         Assert_Truth (Check   => TQ.Is_Empty,
                       Message => "After a full queueing cycle, task queues should be empty again");

         begin
            TQ.Finalize;
         exception
            when others => Fail ("Finalizing an empty task queues should work fine");
         end;

         -- We cannot check that finalization checks work, because the Ada runtime catches that exception

      end Test_Task_Queue;

   begin
      Test_Pending_Counter;
      Test_Task_Queue;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Asynchronous.Executors.Task_Queues;
