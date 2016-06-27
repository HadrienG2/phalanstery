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

package body Phalanstery.Utilities.Signals is

   protected body Signal is

      procedure Send is
      begin
         Sent := True;
      end Send;

      entry Wait when Sent is
      begin
         null;
      end Wait;

   end Signal;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Initial_State is
         S : Signal;
      begin
         select
            S.Wait;
            Fail ("Waiting on a newly created signal should fail");
         else
            null;
         end select;
      end Test_Initial_State;

      procedure Test_Send is
         S : Signal;
      begin
         S.Send;
         select
            S.Wait;
         else
            Fail ("Waiting on a sent signal should succeed");
         end select;
      end Test_Send;

   begin
      Test_Initial_State;
      Test_Send;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Utilities.Signals;
