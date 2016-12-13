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

pragma Warnings (Off);
with Phalanstery;
with Phalanstery.Asynchronous_Jobs;
with Phalanstery.Examples;
with Phalanstery.Examples.Scheduler_Benchmarks;
with Phalanstery.Examples.Trivial_Jobs;
with Phalanstery.Executors;
with Phalanstery.Executors.Interfaces;
with Phalanstery.Executors.Job_Instances;
with Phalanstery.Executors.Job_Instances.References;
with Phalanstery.Executors.Job_Queues;
with Phalanstery.Executors.Job_Queues.References;
with Phalanstery.Executors.SMP.Executor_Objects;
with Phalanstery.Executors.SMP.Specific_Interfaces;
with Phalanstery.Executors.Work_Distribution;
with Phalanstery.Executors.Work_Distribution.Regular;
with Phalanstery.Outcome_Composition;
with Phalanstery.Outcome_Composition.And_Gates;
with Phalanstery.Outcome_Composition.Shorthands;
with Phalanstery.Outcomes;
with Phalanstery.Outcomes.Callbacks;
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Contracts;
with Phalanstery.Outcomes.Implementation;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities;
with Phalanstery.Utilities.Atomic_Counters;
with Phalanstery.Utilities.Debug;
with Phalanstery.Utilities.Group_Waits;
with Phalanstery.Utilities.References;
with Phalanstery.Utilities.References.Not_Null;
with Phalanstery.Utilities.References.Nullable;
with Phalanstery.Utilities.Signals;
with Phalanstery.Utilities.Testing;
pragma Warnings (On);

procedure Main is

   -- The basic idea here is to devise an event-driven model of asynchronous computation, so that a correct program
   -- only needs to block an OS thread for waiting exactly once, when fetching the final result of a computation.
   --
   -- Motto : "Blocking more than once in a program is a bug in the program, the hardware, or the API"

   -- === INPUT AND OUTPUT ===

   -- Passing initial input to an asynchronous task is best done using a task-specific constructor, but there should be
   -- a way to schedule running a task based on input that is not available yet, like hpx::dataflow does.

   -- To exchange data with the outside world on every task iteration, one may use input/output queues called channels.
   -- Senders and listeners would be notified of exceptions (and thus avoid hanging indefinitely) through channel RAII.
   -- A channel input may be associated to multiple outputs, but a misbehaving listener shall not be able to stop the
   -- task. Ideally, a container iterator should be able to behave as a channel, or be easily converted into it, for
   -- in-place data access.

   -- If a notion of a final result is required, it is best represented using a future-like object. HPX' futures may be
   -- used for inspiration.

   -- TODO : Determine a way to pass data in and out of tasks at start, end, and on every iteration
   -- TODO : Determine a composable dataflow model for synchronizing with a task's inputs and outputs
   -- TODO : Add continuations ?
   -- TODO : Think about composition of iterators and data !
   -- TODO : Add data-parallel programming facilities

   -- === MOAR FEATURES ===

   -- TODO : Add non-busy waiting facilities

begin

   -- null; -- DEBUG : Remove scheduler benchmark for coverage tests
   Phalanstery.Examples.Scheduler_Benchmarks.Run_Benchmarks; -- DEBUG : Test scheduling performance using benchmarks

exception
   when E : others =>
      Phalanstery.Utilities.Debug.Last_Chance_Handler ("the main program", E);
end Main;
