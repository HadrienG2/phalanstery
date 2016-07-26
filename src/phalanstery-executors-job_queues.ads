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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Finalization;
with Phalanstery.Executors.Job_Instances.References;
with Phalanstery.Utilities.Atomic_Counters;

package Phalanstery.Executors.Job_Queues is

   -- This package features the data structures that are required in order to schedule job instances, whether these are
   -- ready to run as soon as a CPU core becomes available or waiting for other asynchronous operations to complete.


   -- Ready jobs will be put on a FIFO queue, from which active CPU cores will later fetch them. That FIFO queue is
   -- unbounded because the performance impact of a worker thread blocking on a bounded queue could be catastrophic.
   subtype Job_Instance_Reference is Job_Instances.References.Reference;
   package Ready_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Job_Instance_Reference);
   package Ready_Queue_Implementation is new Ada.Containers.Unbounded_Synchronized_Queues (Ready_Queue_Interfaces);


   -- Waiting jobs do not really follow an organized data structure: a job can start waiting at any time, and resume
   -- execution at any other time, irrespective of what any other job is doing.
   --
   -- For this reason, these jobs will be stored on the heap (see Executors.Scheduling for more details), and the
   -- job queue will only keep track of their amount using a simple atomic conter.
   --
   type Waiting_Counter is tagged limited private;

   procedure Add_Job (Where : in out Waiting_Counter);

   procedure Remove_Job (Where : in out Waiting_Counter);

   function No_Waiting_Job (Where : Waiting_Counter) return Boolean;


   -- A job queue is a combination of a ready job queue and a waiting job counter. These two accounting facilities
   -- are kept separate and not encapsulated, so that a client may synchronize with either part of the queue separately.
   -- This should reduce lock contention as the pending counter and the ready queue are rarely accessed simultaneously.
   type Job_Queue is new Ada.Finalization.Limited_Controlled with
      record
         Ready : Ready_Queue_Implementation.Queue;
         Waiting : Waiting_Counter;
      end record;

   -- This method may be used to wait for a job queue to be free of both ready and waiting jobs
   not overriding procedure Flush (What : Job_Queue);

   -- This method may be used to check that it is the case in a nonblocking fashion
   not overriding function Is_Empty (What : Job_Queue) return Boolean;

   -- At finalization time, the job queue implementation will make sure that a job queue has not been finalized as it
   -- still has jobs in it: this would indicate an error in the executor implementation.
   overriding procedure Finalize (What : in out Job_Queue)
     with Pre => What.Is_Empty;

   -- If the job queue is not empty at finalization time, this exception will be raised, causing a Program_Error.
   Queue_Usage_Error : exception;


   -- Because job queues will be referred to from multiple places, we need some kind of copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   package Atomic_Counters renames Utilities.Atomic_Counters;

   type Waiting_Counter is tagged limited
      record
         Implementation : Atomic_Counters.Atomic_Counter;
      end record;

end Phalanstery.Executors.Job_Queues;
