with Ada.Characters.Latin_1;
with Ada.Task_Identification;
with Ada.Text_IO;

package body Asynchronous.Utilities.Debug is

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

end Asynchronous.Utilities.Debug;
