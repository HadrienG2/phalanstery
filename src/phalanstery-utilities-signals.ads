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

package Phalanstery.Utilities.Signals is

   -- The signal primitive is intended as a lightweight way for one task to have N tasks do something exactly once
   -- A typical use case for it is task shutdown in scenarios where terminate alternatives cannot be used.
   protected type Signal is
      procedure Send;
      entry Wait;
   private
      Sent : Boolean := False;
   end Signal;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Utilities.Signals;
