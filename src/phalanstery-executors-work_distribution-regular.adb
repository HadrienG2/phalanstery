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

with Phalanstery.Utilities.Testing;

package body Phalanstery.Executors.Work_Distribution.Regular is

   overriding function Next_Worker (According_To : in out Method) return Worker_Index is
      Current_Index : Worker_Index renames According_To.Current_Index;
   begin
      return I : constant Worker_Index := Current_Index do
         Current_Index := ((Current_Index - Worker_Index'First + 1)
                           mod Worker_Index'Range_Length) + Worker_Index'First;
      end return;
   end Next_Worker;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Index_Increase is
         M : Method;
         First_Index  : constant Worker_Index := M.Next_Worker;
         Second_Index : constant Worker_Index := M.Next_Worker;
      begin
         if Worker_Index'Last > Worker_Index'First then
            Assert_Truth (Check   => ((Second_Index - First_Index) mod Worker_Index'Range_Length = 1),
                          Message => "Next_Worker should return incrementing worker indices");
         end if;
      end Test_Index_Increase;

      procedure Test_Index_Wraparound is
         M : Method;
         Last_Index    : Worker_Index := M.Next_Worker;
         Current_Index : Worker_Index := M.Next_Worker;
      begin
         while Current_Index > Last_Index loop
            Last_Index := Current_Index;
            Current_Index := M.Next_Worker;
         end loop;
         Assert_Truth (Check   => (Current_Index = Worker_Index'First) and (Last_Index = Worker_Index'Last),
                       Message => "Worker indices should wrap around properly, without forgetting any possibility");
      end Test_Index_Wraparound;

   begin
      Test_Index_Increase;
      Test_Index_Wraparound;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.Work_Distribution.Regular;
