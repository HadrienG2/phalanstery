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

with Phalanstery.Outcome_Composition.Interfaces;
with Phalanstery.Outcomes.Contracts;
with Phalanstery.Outcomes.Interfaces;

package Phalanstery.Asynchronous_Jobs is

   -- An asynchronous job is a user-defined asynchronous operation, which can be used as a more efficient and composable
   -- alternative to Ada tasks, at the cost of a somewhat more complex programming model.
   --
   -- Phalanstery jobs are managed using cooperative multitasking: multiple jobs can be run concurrently on the CPU
   -- cores of the host computer, but on each core, Phalanstery will only switch from one job to another at specific
   -- synchronization points, where the job implementation tells the Phalanstery runtime what should be done next.
   --
   -- This is what allows one to spawn a lot of Phalanstery jobs without wasting a large amount of system ressources,
   -- but that power comes at a price: Phalanstery jobs should never directly use any API that can block the underlying
   -- OS thread. Instead, synchronization with other asynchronous operations should be achieved using outcome objects:
   --
   subtype Valid_Outcome_Client is Outcomes.Contracts.Valid_Outcome_Client;
   subtype Valid_Outcome_List is Outcome_Composition.Interfaces.Valid_Outcome_List;

   -- Asynchronous job workloads are implemented as subprograms. On every call, these subprograms return a value to the
   -- Phalanstery scheduler, telling it what should be done next. In addition, jobs may also raise Ada exceptions, that
   -- will be propagated to operations depending on that job using the associated outcome object.
   --
   -- Jobs can currently request the following from the Phalanstery scheduler:
   --    - Consider the job to be completed, stop running it and notify its clients ("Finished" status)
   --    - Give other jobs some time to run, and fetch new scheduling information ("Yielding" status)
   --    - Wait for other asynchronous operations to complete before resuming job operation ("Waiting" status)
   --    - Cancel the job, and notify the clients that it will never complete ("Canceled" status)
   --
   -- The job statuses discussed above are public, but job return values are kept private to ease future extensions:
   --
   type Return_Status is (Finished, Yielding, Waiting, Canceled);
   type Return_Value (<>) is private;

   -- Job return values are created in the following way...
   Return_Finished : constant Return_Value;
   Return_Yielding : constant Return_Value;
   Return_Canceled : constant Return_Value;
   function Return_Waiting (Cause : Valid_Outcome_Client) return Return_Value;
   function Return_Waiting (Cause : Valid_Outcome_List) return Return_Value;

   -- ...and can be queried in the following way
   function Status (What : Return_Value) return Return_Status;
   function Awaited_Outcome (What : Return_Value) return Valid_Outcome_Client
     with Pre => (Status (What) = Waiting);

   -- To allow asynchronous jobs to retain state across invocations, they are implemented as Ada tagged types inheriting
   -- from a common interface. By overriding that interface's subprograms, a concrete job type can define what happens
   -- when a job runs normally, is cancelled, or when the job's dependencies fail.
   --
   -- To comply with Ada accessibility rules, job types should be defined at global scope. Otherwise, the underlying Ada
   -- implementation will consider the program to be invalid, as an asynchronous job object might outlive its type.
   --
   type Asynchronous_Job is interface;

   -- The "Run" method is the heart of an asynchronous job. From the point where a job is ready to run, it will be
   -- repeatedly called until the job completes, fails, or is canceled. The Was_Canceled parameter is used to notify a
   -- job that it has been canceled by a client while it was running, allowing for early termination.
   -- Run communicates with the job scheduler through its return values, using the protocol defined above.
   function Run (Who          : in out Asynchronous_Job;
                 Was_Canceled : Boolean) return Return_Value is abstract;

   -- By default, Phalanstery assumes that a waiting job is always in a consistent state: if one of the asynchronous
   -- operations that the job depends on fails to run to completion, either due to cancelation or errors, it is assumed
   -- that stopping the job immediately is the best possible course of action.
   --
   -- When it isn't the case, this default behaviour can be changed by overriding the following hook. Please note that
   -- this should be done with appropriate care:
   --    - Dependency error handling code should be written under the assumption that all the job's dependencies are in
   --      an erronerous, inconsistent state, and not rely on the results of these operations. In general, all that this
   --      code can and should do is clean up the job's internal state and terminate.
   --    - If a job waits for asynchronous operations multiple times in its run cycle, this hook should be callable at
   --      any of these synchronization points, for example by using case statements and a state tracking variable.
   --
   subtype Aborted_Outcome_Status is Outcomes.Interfaces.Aborted_Outcome_Status;
   procedure Handle_Aborted_Dependency (Who               : in out Asynchronous_Job;
                                        Dependency_Status : Aborted_Outcome_Status) is null;

   -- If a job dependency fails, the job's outcome object will be marked with the following error:
   Dependency_Error : exception;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Return_Value (State : Return_Status) is
      record
         case State is
            when Finished | Yielding | Canceled =>
               null;
            when Waiting =>
               Awaited_Outcome : Valid_Outcome_Client;
         end case;
      end record;

   Return_Finished : constant Return_Value := (State => Finished);
   Return_Yielding : constant Return_Value := (State => Yielding);
   Return_Canceled : constant Return_Value := (State => Canceled);

end Phalanstery.Asynchronous_Jobs;
