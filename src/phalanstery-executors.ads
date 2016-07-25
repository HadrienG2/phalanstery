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

package Phalanstery.Executors with Pure is

   -- Executor objects are in charge of scheduling and running asynchronous jobs. One sends a job to an executor, along
   -- with some scheduling information indicating when and where it should be scheduled. In return, the executor makes
   -- a copy of the job, prepares it for execution, and provides the caller with an outcome object that can be used to
   -- synchronize with the newly scheduled asynchronous operation.
   --
   -- Several kinds of executor objects may be envisioned, corresponding to various processing hardware topologies:
   --    - SMP (symmetric multiprocessing), where all processing nodes are considered equivalent.
   --    - NUMA (non-uniform memory access), where jobs are best located on some specific processing nodes, but can run
   --                                        unmodified on other processing nodes at the cost of lower performance.
   --    - Distributed, where jobs cannot run on another processing locality than the one they were assigned to, without
   --                   having all the memory objects they access being explicitly migrated from one place to another.
   --
   -- Ideally, these kinds of executors should be composable into relatively deep processing hierarchies, such as:
   --    - A network of distributed processing nodes (computing cluster)...
   --    - ...each of which features some internal distribution of processing resources (CPU, GPU, MIC)...
   --    - ...on the inside of which one can find multiple NUMA localities (physical processors, GPU computing units)...
   --    - ...each of which exhibits some internal symmetric parallelism.
   --
   -- Processing topologies are strongly related to memory topologies, support for which is not yet implemented in
   -- Phalanstery. Consequently, only SMP executors are implemented at the moment.
   --
   -- The children of this package are organized as follows:
   --    - Executors.Interfaces defines the part of the executor interface that has been made topology-agnostic.
   --    - Executors.Job_Instances provides a representation of running asynchronous jobs.
   --    - Executors.Job_Queues provides a thread-safe FIFO queue, used to store scheduled jobs.
   --    - Executors.Scheduling defines the job scheduling logic internally used by executors.
   --    - Executors.SMP holds the current executor object implementation, which is SMP specific.

end Phalanstery.Executors;
