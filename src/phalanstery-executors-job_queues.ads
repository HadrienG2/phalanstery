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

   -- Ready asynchronous jobs will be put on a FIFO queue, and executed once worker threads become available.
   -- Said queue is unbounded because the performance impact of a worker thread blocking on a bounded queue
   -- would be catastrophic.
   subtype Job_Instance_Reference is Job_Instances.References.Reference;
   package Ready_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Job_Instance_Reference);
   package Ready_Queue_Implementation is new Ada.Containers.Unbounded_Synchronized_Queues (Ready_Queue_Interfaces);


   -- Pending jobs do not really follow an organized queue-like structure: a job may become pending at any time, and
   -- resume execution at any other time, irrespective of what any other job is doing.
   --
   -- For this reason, pending jobs will be stored on the heap (see Executors.Scheduling for more details), and the
   -- job queue will only keep track of their amount using an atomic conter.
   --
   type Pending_Counter is tagged limited private;

   procedure Add_Job (Where : in out Pending_Counter);

   procedure Remove_Job (Where : in out Pending_Counter);

   function No_Pending_Job (Where : Pending_Counter) return Boolean;


   -- A job queue is a combination of a ready job queue and a pending job counter. These two accounting facilities
   -- are kept separate and un-encapsulated so that a client may synchronize with either part of the queue separately,
   -- which should reduce lock contention as the pending counter and the ready queue are rarely accessed simultaneously.
   --
   -- Job queue users should make sure that all pending and ready jobs have completed before finalizing a queue,
   -- otherwise disaster will ensue. The job queue implementation will attempt to detect this error and report it.
   -- Unfortunately, due to Ada's policy on finalization exceptions, this means Program_Error will be raised.
   --
   type Job_Queue is new Ada.Finalization.Limited_Controlled with
      record
         Ready : Ready_Queue_Implementation.Queue;
         Pending : Pending_Counter;
      end record;

   not overriding procedure Flush (What : Job_Queue);

   not overriding function Is_Empty (What : Job_Queue) return Boolean;

   overriding procedure Finalize (What : in out Job_Queue)
     with Pre => What.Is_Empty;

   Queue_Usage_Error : exception;

   -- Because job queues will be referred to from multiple places, we need some kind of copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   package Atomic_Counters renames Utilities.Atomic_Counters;

   type Pending_Counter is tagged limited
      record
         Implementation : Atomic_Counters.Atomic_Counter;
      end record;

end Phalanstery.Executors.Job_Queues;
