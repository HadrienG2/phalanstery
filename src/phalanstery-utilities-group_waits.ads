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

package Phalanstery.Utilities.Group_Waits is

   -- This primitive is intended as a way for one task to wait for a group of N tasks to do something.
   -- It can typically be used in abort-less task termination: the master task starts the termination of its slaves
   -- using a signal, and waits for them to be finished using a group wait.
   protected type Group_Wait (Number_Of_Tasks : Natural) is
      procedure Mark_One_Ready;
      entry Wait_All;
   private
      Ready_Tasks : Natural := 0;
   end Group_Wait;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Utilities.Group_Waits;
