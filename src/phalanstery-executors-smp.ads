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

package Phalanstery.Executors.SMP with Pure is

   -- The current implementation of Phalanstery executors is heavily skewed towards symmetric multiprocessing (SMP)
   -- hardware, and not well-suited to more complex processing hardware topologies. We have tried to isolate the
   -- SMP-specific part of the implementation as children to this package, to clarify what is going to change in the
   -- future and should not be relied upon.
   --
   -- The children of this package are organized as follows:
   --    - SMP.Specific_Interfaces defines an executor interface suitable for SMP hardware.
   --    - SMP.Executor_Tasks provides a low-level task-based executor implementation.
   --    - SMP.Executor_Objects provides a high-level implementation of the executor interface.

end Phalanstery.Executors.SMP;
