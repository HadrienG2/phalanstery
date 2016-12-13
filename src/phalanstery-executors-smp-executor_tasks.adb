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

with Phalanstery.Asynchronous_Jobs;
with Phalanstery.Examples.Trivial_Jobs;
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Executors.Scheduling;
with Phalanstery.Executors.Job_Instances.References;
with Phalanstery.Executors.Job_Queues.References;
with Phalanstery.Executors.Work_Distribution;
with Phalanstery.Executors.Work_Distribution.Regular;
with Phalanstery.Utilities.Debug;
with Phalanstery.Utilities.Group_Waits;
with Phalanstery.Utilities.Signals;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.SMP.Executor_Tasks is

   subtype Valid_Job_Instance is Job_Instances.References.Valid_Reference;
   subtype Valid_Job_Queue is Job_Queues.References.Valid_Reference;
   use all type Outcomes.Interfaces.Outcome_Status;

   task body Executor_Task is

      -- On the inside, an executor task manages a pool of worker threads
      subtype Worker_Index is Specific_Interfaces.Worker_Count range 1 .. Number_Of_Workers;
      use type Worker_Index;

      -- Executor tasks distribute ready jobs on a set of FIFO queues, one per worker thread
      type Job_Queue_Array is array (Worker_Index) of Valid_Job_Queue;
      Worker_Job_Queues : Job_Queue_Array;  -- DEBUG: Avoided anonymous array type to work around GNAT bug

      -- To do so, they use a work distribution scheme that is selected here
      package Worker_Distribution is new Work_Distribution (Worker_Index);
      package Worker_Distribution_Implementation is new Worker_Distribution.Regular;
      Worker_Distribution_Method : Worker_Distribution_Implementation.Method;

      -- Because worker threads are commanded using protected objects rather than task entries, terminate alternatives
      -- cannot be used. Instead, we go through a manual termination procedure: the executor task requests worker
      -- termination using a signal object, and acknowledges it using a group wait object.
      Stop_Request : Utilities.Signals.Signal;
      Worker_Wait : Utilities.Group_Waits.Group_Wait (Natural (Number_Of_Workers));

      -- Work items are executed by a flock of worker threads, which are defined as follows
      task type Worker (Index : Worker_Index) with Priority => System.Priority'Last;
      task body Worker is

         -- Each worker has a private work queue to draw from
         Job_Queue : Valid_Job_Queue renames Worker_Job_Queues (Index);

         -- This function runs a work item and tells whether it is yielding or not
         function Run_Work_Item (What : Valid_Job_Instance) return Boolean is
            use all type Asynchronous_Jobs.Return_Status;
            Work_Item_Yielding : Boolean := False;
         begin
            declare
               Job_Canceled : constant Boolean := What.Get.Outcome.Is_Canceled;
               Work_Item_Output : constant Asynchronous_Jobs.Return_Value :=
                 What.Get.Job_Object.Run (Was_Canceled => Job_Canceled);
            begin
               case Asynchronous_Jobs.Status (Work_Item_Output) is
                  when Finished =>
                     What.Set.Outcome.Mark_Done;
                  when Yielding =>
                     Work_Item_Yielding := True;
                  when Canceled =>
                     What.Set.Outcome.Cancel;
                  when Waiting =>
                     Scheduling.Schedule_Job (Who   => What,
                                              After => Asynchronous_Jobs.Awaited_Outcome (Work_Item_Output),
                                              On    => Job_Queue);
               end case;
               return Work_Item_Yielding;
            end;
         exception
            when E : others =>
               What.Set.Outcome.Mark_Error (E);
               return False;
         end Run_Work_Item;

         -- This function processes a work item according to the current scheduling policy
         procedure Process_Work_Item (What : Valid_Job_Instance) with Inline is
         begin
            case Active_Scheduling_Policy is
               when Round_Robin =>
                  if Run_Work_Item (What) then
                     Job_Queue.Set.Ready.Enqueue (What);
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

         -- Process ready jobs, and wait for them unless the stop signal is set
         while Worker_Active loop
            declare
               Work_Item : Job_Instances.References.Reference;
            begin
               select
                  Job_Queue.Set.Ready.Dequeue (Work_Item);
                  Process_Work_Item (Work_Item);
               then abort
                  Stop_Request.Wait;
                  Worker_Active := False;
               end select;
            end;
         end loop;

         -- Signal the underlying executor task that all work is complete
         Worker_Wait.Mark_One_Ready;

      exception
         when E : others =>
            Utilities.Debug.Last_Chance_Handler ("an asynchronous worker", E);
      end Worker;

      -- We define the following flock of workers
      Workers : array (Worker_Index) of access Worker;

      -- Executor tasks will wait for work until this flag goes to False
      Executor_Active : Boolean := True;

   begin

      -- Initialize worker threads
      for I in Worker_Index loop
         Workers (I) := new Worker (Index => I);
      end loop;

      -- Accept requests from the outside world until requested to stop
      while Executor_Active loop
         select
            accept Schedule_Job (What    : Interfaces.Any_Asynchronous_Job;
                                 After   : Interfaces.Valid_Outcome_Client;
                                 Outcome : out Interfaces.Valid_Outcome_Client) do
               declare
                  Work_Item   : constant Valid_Job_Instance := Job_Instances.References.Make_Job_Instance (What);
                  Next_Worker : constant Worker_Index := Worker_Distribution_Method.Next_Worker;
               begin
                  Outcome := Work_Item.Get.Outcome.Make_Client;
                  Scheduling.Schedule_Job (Who   => Work_Item,
                                           After => After,
                                           On    => Worker_Job_Queues (Next_Worker));
               end;
            end Schedule_Job;
         or
            accept Stop do
               for Queue of Worker_Job_Queues loop
                  Queue.Set.Flush;
               end loop;
               Stop_Request.Send;
               Executor_Active := False;
               Worker_Wait.Wait_All;
            end Stop;
         end select;
      end loop;

   exception
      when E : others =>
         Utilities.Debug.Last_Chance_Handler ("an asynchronous executor", E);
   end Executor_Task;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      subtype Outcome_Client is Outcomes.Clients.Client;

      Number_Of_Workers : constant := 2;

      Ready_Server : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
      Ready_Client : constant Interfaces.Valid_Outcome_Client := Ready_Server.Make_Client;

      procedure Setup_Tests is
      begin
         Ready_Server.Mark_Done;
      end Setup_Tests;

      procedure Test_Null_Job is
         Executor : Executor_Task (Number_Of_Workers);
         T : Examples.Trivial_Jobs.Null_Job;
         Client : Outcome_Client;
      begin
         select
            Executor.Schedule_Job (What    => T,
                                   After   => Ready_Client,
                                   Outcome => Client);
         or
            delay 0.02;
            Fail ("An executor should accept jobs quickly");
         end select;
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("The null job should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Done),
                       Message => "The null job should appear completed after its executor has terminated");
      end Test_Null_Job;

      procedure Test_Self_Canceling_Job is
         Executor : Executor_Task (Number_Of_Workers);
         T : Examples.Trivial_Jobs.Self_Canceling_Job;
         Client : Outcome_Client;
      begin
         select
            Executor.Schedule_Job (What    => T,
                                   After   => Ready_Client,
                                   Outcome => Client);
         or
            delay 0.02;
            Fail ("An executor should accept jobs quickly");
         end select;
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("The self-canceling job should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Canceled),
                       Message => "The self-canceling job should appear canceled after its executor has terminated");
      end Test_Self_Canceling_Job;

      procedure Test_Yielding_Job is
         Executor : Executor_Task (Number_Of_Workers);
         T : Examples.Trivial_Jobs.Yielding_Job (1);
         Client : Outcome_Client;
      begin
         Executor.Schedule_Job (What    => T,
                                After   => Ready_Client,
                                Outcome => Client);
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("A simple yielding job should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Done),
                       Message => "The yielding job should appear completed after its executor has terminated");
      end Test_Yielding_Job;

      procedure Test_Erronerous_Job is
         Executor : Executor_Task (Number_Of_Workers);
         T : Examples.Trivial_Jobs.Erronerous_Job (1);
         Client : Outcome_Client;
      begin
         Executor.Schedule_Job (What    => T,
                                After   => Ready_Client,
                                Outcome => Client);
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("The erronerous job should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Error),
                       Message => "The erronerous job should go to the Error state after its executor has terminated");
      end Test_Erronerous_Job;

      procedure Test_Canceled_Wait_Job is
         Executor : Executor_Task (Number_Of_Workers);
         T : Examples.Trivial_Jobs.Canceled_Wait_Job (1);
         Client : Outcome_Client;
      begin
         Executor.Schedule_Job (What    => T,
                                After   => Ready_Client,
                                Outcome => Client);
         select
            Executor.Stop;
         or
            delay 0.02;
            Fail ("A job waiting for a canceled operation should execute instantly");
         end select;
         Assert_Truth (Check   => (Client.Status = Canceled),
                       Message => "The canceled-wait job should appear canceled after its executor has terminated");
      end Test_Canceled_Wait_Job;

   begin
      Setup_Tests;
      Test_Null_Job;
      Test_Self_Canceling_Job;
      Test_Yielding_Job;
      Test_Erronerous_Job;
      Test_Canceled_Wait_Job;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.SMP.Executor_Tasks;
