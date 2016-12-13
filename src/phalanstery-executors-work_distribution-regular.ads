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
package Phalanstery.Executors.Work_Distribution.Regular is

   -- The regular work distribution method simply works by cycling through the available workers.
   -- It is very simple, predictable and CPU-efficient, but can lead to load imbalance on pathological workloads.

   type Method is new Method_Interface with private;

   overriding function Next_Worker (According_To : in out Method) return Worker_Index;

private

   type Method is new Method_Interface with
      record
         Current_Index : Worker_Index := Worker_Index'First;
      end record;

end Phalanstery.Executors.Work_Distribution.Regular;
