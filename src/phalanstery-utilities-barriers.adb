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

package body Phalanstery.Utilities.Barriers is

   protected body Barrier is

      procedure Join is
      begin
         Ready_Tasks := Ready_Tasks + 1;
      end Join;

      entry Wait when Ready_Tasks = Number_Of_Tasks is
      begin
         null;
      end Wait;

   end Barrier;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Null_Barrier is
         B : Barrier (0);
      begin
         select
            B.Wait;
         else
            Fail ("The null barrier should be opened");
         end select;
      end Test_Null_Barrier;

      procedure Test_One_Task is
         B : Barrier (1);
      begin
         select
            B.Wait;
            Fail ("The barrier should be initially closed");
         else
            null;
         end select;
         B.Join;
         select
            B.Wait;
         else
            Fail ("Waiting on a full barrier should succeed");
         end select;
      end Test_One_Task;

      procedure Test_Two_Tasks is
         B : Barrier (2);
      begin
         B.Join;
         select
            B.Wait;
            Fail ("One arrival should not be enough to trigger the barrier");
         else
            null;
         end select;
         B.Join;
         select
            B.Wait;
         else
            Fail ("Waiting on a full barrier should succeed");
         end select;
      end Test_Two_Tasks;

   begin
      Test_Null_Barrier;
      Test_One_Task;
      Test_Two_Tasks;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Utilities.Barriers;
