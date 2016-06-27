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

with Ada.Assertions;
with Ada.Text_IO;

package body Phalanstery.Utilities.Testing is

   procedure Startup_Test (How : access procedure) is
   begin
      if Run_Tests_On_Startup then
         How.all;
      end if;
   end Startup_Test;

   procedure Assert_Truth (Check   : Boolean;
                           Message : String) is
   begin
      if not Check then
         Fail (Message);
      end if;
   end Assert_Truth;

   procedure Fail (Message : String) is
   begin
      Ada.Text_IO.Put_Line ("ASSERTION FAILED : " & Message);
      raise Ada.Assertions.Assertion_Error;
   end Fail;

end Phalanstery.Utilities.Testing;
