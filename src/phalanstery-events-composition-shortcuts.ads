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

package Phalanstery.Events.Composition.Shortcuts is

   -- In the simplest use cases, the whole AND gate life cycle may be shortened into the following convenience function.
   -- For more complicated use cases, see the And_Gates sibling package for raw access to the AND gate implementation
   function When_All (Wait_List : Valid_Event_List) return Valid_Event_Client;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Events.Composition.Shortcuts;
