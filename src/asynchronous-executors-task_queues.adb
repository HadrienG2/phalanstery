with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Executors.Task_Queues is

   use type Ada.Containers.Count_Type;

   protected body Pending_Counter is

      procedure Add_Task is
      begin
         Count := Count + 1;
      end Add_Task;

      procedure Remove_Task is
      begin
         Count := Count - 1;
      end Remove_Task;

      function No_Pending_Task return Boolean is (Count = 0);

      entry Flush_Pending when No_Pending_Task is
      begin
         null;
      end Flush_Pending;

   end Pending_Counter;

   not overriding function Is_Empty (What : Task_Queue) return Boolean is
     ((What.Ready.Current_Use = 0) and (What.Pending.No_Pending_Task));

   overriding procedure Finalize (What : in out Task_Queue) is
   begin
      if not What.Is_Empty then
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

         select
            C.Flush_Pending;
         else
            Fail ("Flush_Pending should not be initially blocking");
         end select;

         C.Add_Task;
         Assert_Truth (Check   => (not C.No_Pending_Task),
                       Message => "After adding a pending task, counters should not signal it");

         select
            C.Flush_Pending;
            Fail ("Flushing a non-empty pending counter should be blocking");
         else
            null;
         end select;

         C.Remove_Task;
         Assert_Truth (Check   => C.No_Pending_Task,
                       Message => "After deleting the pending tasks, counters, should go back to the empty state");

         select
            C.Flush_Pending;
         else
            Fail ("After an add/remove round trip, flushing pending task counters should go back to non-blocking");
         end select;

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
