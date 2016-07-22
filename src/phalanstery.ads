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

package Phalanstery with Pure is

   -- Phalanstery is an Ada library which helps one efficiently perform parallel and asynchronous computations.
   --
   -- The main functionality is divided into the following child packages:
   --    - Phalanstery.Asynchronous_Jobs specifies how one can define asynchronous jobs.
   --    - Phalanstery.Executors provides a mean to run such asynchronous jobs.
   --    - Phalanstery.Outcomes provides a way to monitor and cancel asynchronous operations (jobs, IO...).
   --    - Phalanstery.Outcome_Composition provides a way to monitor multiple operations at once.
   --    - Phalanstery.Utilities provides some simple Ada components, used as a building block for these facilities.
   --
   -- In addition to this core infrastructure, the following packages provide some extra functionality:
   --    - Phalanstery.Examples gives some examples outlining how the library can be used.

end Phalanstery;
