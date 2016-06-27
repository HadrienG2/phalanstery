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

with Phalanstery.Executors.Interfaces;
with System;

private package Phalanstery.Executors.Executor_Tasks is

   -- Under the hood, executor objects spawn and manage an Ada task, which is called the executor task.
   task type Executor_Task (Number_Of_Workers : Interfaces.Worker_Count) with Priority => System.Priority'First is

      -- Queue a new asynchronous job
      entry Schedule_Job (What  : Interfaces.Any_Async_Job;
                          After : Interfaces.Event_Wait_List;
                          Event : out Interfaces.Valid_Event_Client);

      -- Schedule executor termination
      entry Stop;

   end Executor_Task;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   -- Job executors can currently operate according to one of two statically selected scheduling policies:
   --    - In batch mode, executors run jobs as long as they can, which maximizes computational performance.
   --    - In round-robin mode, executors switch between jobs in a cyclic FIFO fashion, which minimizes starvation.
   type Scheduling_Policy is (Batch, Round_Robin);
   Active_Scheduling_Policy : constant Scheduling_Policy := Batch;

end Phalanstery.Executors.Executor_Tasks;
