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

   -- This package, together with its children, provides a way to run computations asynchronously.
   --
   -- Its children provide the following facilities:
   --    - Phalanstery.Jobs provides an interface for user-defined asynchronous jobs.
   --    - Phalanstery.Executors provides a mean to run such asynchronous jobs.
   --    - Phalanstery.Outcomes provides an interface to monitor and cancel asynchronous operations (jobs, IO...).
   --    - Phalanstery.Outcome_Composition provides a way to compose outcome objects, e.g. using an AND relationship.
   --    - Phalanstery.Utilities provides some basic infrastructure for these facilities.

end Phalanstery;
