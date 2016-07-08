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

with Ada.Exceptions;

package Phalanstery.Utilities.Debug is

   -- This function pretty-prints a debug message
   procedure Display (Message : String);

   -- This function pretty-prints a warning about an unhandled exception in a task
   procedure Display_Unhandled_Exception (Task_Name : String;
                                          Error     : Ada.Exceptions.Exception_Occurrence);

   -- This function prints details about an unhandled exception in a task, then terminates the program
   procedure Last_Chance_Handler (Task_Name : String;
                                  Error     : Ada.Exceptions.Exception_Occurrence);

end Phalanstery.Utilities.Debug;
