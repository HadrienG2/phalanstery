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
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Utilities.Group_Waits is

   protected body Group_Wait is

      procedure Mark_One_Ready is
      begin
         Ready_Tasks := Ready_Tasks + 1;
      end Mark_One_Ready;

      entry Wait_All when Ready_Tasks = Number_Of_Tasks is
      begin
         null;
      end Wait_All;

   end Group_Wait;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Empty_Group_Wait is
         B : Group_Wait (0);
      begin
         select
            B.Wait_All;
         else
            Fail ("Waiting for an empty group should return immediately");
         end select;
      end Test_Empty_Group_Wait;

      procedure Test_One_Task is
         B : Group_Wait (1);
      begin
         select
            B.Wait_All;
            Fail ("Waiting for one non-ready task should block");
         else
            null;
         end select;
         B.Mark_One_Ready;
         select
            B.Wait_All;
         else
            Fail ("Waiting for one ready task should return immediately");
         end select;
      end Test_One_Task;

      procedure Test_Two_Tasks is
         B : Group_Wait (2);
      begin
         B.Mark_One_Ready;
         select
            B.Wait_All;
            Fail ("One ready task should not be enough to consider the wait over");
         else
            null;
         end select;
         B.Mark_One_Ready;
         select
            B.Wait_All;
         else
            Fail ("When all tasks are ready, the wait should succeed");
         end select;
      end Test_Two_Tasks;

   begin
      Test_Empty_Group_Wait;
      Test_One_Task;
      Test_Two_Tasks;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Utilities.Group_Waits;
