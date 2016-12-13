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

generic
   type Worker_Index is range <>;
package Phalanstery.Executors.Work_Distribution is

   -- This package defines a set of methods for distributing work across a pool of numbered workers
   type Method_Interface is interface;

   -- A work distribution method iteratively tells which (numbered) worker should run the next task
   function Next_Worker (According_To : in out Method_Interface) return Worker_Index is abstract;

end Phalanstery.Executors.Work_Distribution;
