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
with Phalanstery.Outcomes.Contracts;

package Phalanstery.Examples.Trivial_Jobs is

   -- This package defines basic examples of asynchronous jobs, intended for use in unit tests or microbenchmarks.

   -- Let us define some convenience notations first
   subtype Asynchronous_Job is Asynchronous_Jobs.Asynchronous_Job;

   -- Anytime one of these trivial jobs voluntarily raises an exception, it will be this one
   Expected_Error : exception;

   -- This job completes immediately, it can be used to measure job scheduling and queue contention overhead
   type Null_Job is new Asynchronous_Job with null record;
   overriding function Run (Who          : in out Null_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job yields in a tight loop, it can be used to measure yielding overhead
   type Yielding_Job (Iterations : Natural) is new Asynchronous_Job with
      record
         Counter : Natural := 0;
      end record;
   overriding function Run (Who          : in out Yielding_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while then returns, it can be used to measure how long jobs should last in order to
   -- minimize scheduler and executor overhead. We will use this technique in all the remaining jobs.
   type Waiting_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with null record;
   overriding function Run (Who          : in out Waiting_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while, then aborts with an exception
   type Erronerous_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with null record;
   overriding function Run (Who          : in out Erronerous_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while, then waits for a finished operation, then busy-waits some more, then finishes
   type Ready_Wait_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with
      record
         Has_Waited : Boolean := False;
      end record;
   overriding function Run (Who          : in out Ready_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while, then waits for a canceled operation, and will never finish
   type Canceled_Wait_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with null record;
   overriding function Run (Who          : in out Canceled_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while, then waits for an erronerous operation, and will never finish
   type Error_Wait_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with null record;
   overriding function Run (Who          : in out Error_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while, then waits for an outcome object that was specified at initialization time
   type Custom_Wait_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with
      record
         Target : Outcomes.Contracts.Valid_Outcome_Client;
      end record;
   overriding function Run (Who          : in out Custom_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

   -- This job busy-waits for a while, then cancels an asynchronous operation and returns
   type Cancelation_Job (Waiting_Microseconds : Natural) is new Asynchronous_Job with
      record
         Target : Outcomes.Contracts.Valid_Outcome_Client;
      end record;
   overriding function Run (Who          : in out Cancelation_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value;

private

   -- This is the busy-waiting routine used by all busy-waiting jobs
   procedure Busy_Wait (Microseconds : Natural);

end Phalanstery.Examples.Trivial_Jobs;
