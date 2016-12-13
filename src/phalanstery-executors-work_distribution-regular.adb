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

package body Phalanstery.Executors.Work_Distribution.Regular is

   overriding function Next_Worker (According_To : in out Method) return Worker_Index is
      Current_Index : Worker_Index := According_To.Current_Index;
   begin
      return I : constant Worker_Index := Current_Index do
         Current_Index := ((Current_Index - Worker_Index'First + 1)
                           mod Worker_Index'Range_Length) + Worker_Index'First;
      end return;
   end Next_Worker;

end Phalanstery.Executors.Work_Distribution.Regular;
