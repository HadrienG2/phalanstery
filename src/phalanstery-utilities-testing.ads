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

package Phalanstery.Utilities.Testing is

   -- This package provides unit testing facilities which facilitate spotting regressions in the system

   -- This procedure may be used to conditionally run tests for all imported modules on application startup
   procedure Startup_Test (How : access procedure);

   -- This procedure asserts that a boolean predicate is true, else aborts after displaying a rationale
   procedure Assert_Truth (Check   : Boolean;
                           Message : String);

   -- This procedure represents a failed test
   procedure Fail (Message : String);

private

   -- Set this flag to True in order to enable Startup_Test.
   Run_Tests_On_Startup : Boolean := True;

end Phalanstery.Utilities.Testing;
