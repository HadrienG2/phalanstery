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

   -- To execute asynchronous jobs, one must send them to executor objects. These will take care of running the job.
   -- In the future, executors might span multiple physical machines and perform distributed load balancing.
   --
   -- The children of this package are organized as follows :
   --    - Executors.Interfaces defines the common interface design followed by executors.
   --    - Executors.Job_Instances defines a way to package running asynchronous jobs.
   --    - Executors.Job_Queues defines data structures for the manadement of pending jobs.
   --    - Executors.Scheduling defines the job scheduling logic of executors.
   --    - Executors.Executor_Tasks provides a task-based implementation of the executor concept.
   --    - Executors.Objects provides a high-level interface to executor tasks

end Phalanstery.Executors;
