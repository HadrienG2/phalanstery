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

with Ada.Characters.Latin_1;
with Ada.Task_Identification;
with Ada.Text_IO;

package body Phalanstery.Utilities.Debug is

   procedure Display (Message : String) is
   begin
      Ada.Text_IO.Put_Line ("DEBUG: " & Message);
   end Display;

   procedure Display_Unhandled_Exception (Task_Name : String;
                                          Error     : Ada.Exceptions.Exception_Occurrence) is
   begin
      Display ("Unhandled exception in " & Task_Name &
                 " (task id """ & Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task) &
                 """)!" & Ada.Characters.Latin_1.LF &
                 Ada.Exceptions.Exception_Information (Error));
   end Display_Unhandled_Exception;

   procedure Last_Chance_Handler (Task_Name : String;
                                  Error     : Ada.Exceptions.Exception_Occurrence) is
   begin
      Display_Unhandled_Exception (Task_Name, Error);
      Ada.Task_Identification.Abort_Task (Ada.Task_Identification.Environment_Task);
   end Last_Chance_Handler;

end Phalanstery.Utilities.Debug;
