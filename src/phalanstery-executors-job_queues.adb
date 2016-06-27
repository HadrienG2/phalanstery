-- Copyright 2016 Hadrien Grasland
--
-- This file is part of Phalanstery.
--
-- Phalanstery is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Phalanstery is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Phalanstery.  If not, see <http://www.gnu.org/licenses/>.

with Phalanstery.Utilities.Debug;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.Job_Queues is

   use type Ada.Containers.Count_Type;

   procedure Add_Job (Where : in out Pending_Counter) is
   begin
      Atomic_Counters.Increment (Where.Implementation);
   end Add_Job;

   procedure Remove_Job (Where : in out Pending_Counter) is
      Negative_Pending_Count : constant Boolean := Atomic_Counters.Decrement (Where.Implementation);
   begin
      pragma Assert (not Negative_Pending_Count, "The amount of pending jobs should never become negative!");
   end Remove_Job;

   function No_Pending_Job (Where : Pending_Counter) return Boolean is
      (Atomic_Counters.Is_One (Where.Implementation));

   procedure Flush (What : Job_Queue) is
   begin
      while not What.Is_Empty loop
         delay 0.05;
      end loop;
   end Flush;

   not overriding function Is_Empty (What : Job_Queue) return Boolean is
     ((What.Ready.Current_Use = 0) and (What.Pending.No_Pending_Job));

   overriding procedure Finalize (What : in out Job_Queue) is
   begin
      if not What.Is_Empty then
         Utilities.Debug.Display ("A job queue was discarded as it still had jobs in it!");
         raise Queue_Usage_Error;
      end if;
   end Finalize;

   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Pending_Counter is
         C : Pending_Counter;
      begin

         Assert_Truth (Check   => C.No_Pending_Job,
                       Message => "Pending counters should initially feature no pending job");

         C.Add_Job;
         Assert_Truth (Check   => (not C.No_Pending_Job),
                       Message => "After adding a pending job, counters should not signal it");

         C.Remove_Job;
         Assert_Truth (Check   => C.No_Pending_Job,
                       Message => "After deleting the pending jobs, counters, should go back to the empty state");

      end Test_Pending_Counter;

      procedure Test_Job_Queue is
         TI : Job_Instance_Reference;
         TQ : Job_Queue;
      begin

         Assert_Truth (Check   => TQ.Is_Empty,
                       Message => "Job queues should be born empty");

         TQ.Pending.Add_Job;
         Assert_Truth (Check   => (not TQ.Is_Empty),
                       Message => "Job queues with a pending job should not be empty");

         TQ.Ready.Enqueue (TI);
         Assert_Truth (Check   => (not TQ.Is_Empty),
                       Message => "Job queues with a pending job and a queued job should not be empty");

         TQ.Pending.Remove_Job;
         Assert_Truth (Check   => (not TQ.Is_Empty),
                       Message => "Job queues with a queued job should not be empty");

         TQ.Ready.Dequeue (TI);
         Assert_Truth (Check   => TQ.Is_Empty,
                       Message => "After a full queueing cycle, job queues should be empty again");

         begin
            TQ.Finalize;
         exception
            when others => Fail ("Finalizing an empty job queue should work fine");
         end;

         -- We cannot check that finalization checks work, because the Ada runtime catches that exception

      end Test_Job_Queue;

   begin
      Test_Pending_Counter;
      Test_Job_Queue;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.Job_Queues;
