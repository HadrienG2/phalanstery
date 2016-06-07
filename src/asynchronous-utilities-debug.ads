with Ada.Exceptions;

package Asynchronous.Utilities.Debug is

   -- This function pretty-prints a debug message
   procedure Display (Message : String);

   -- This function pretty-prints a warning about an unhandled exception in a task
   procedure Display_Unhandled_Exception (Task_Name : String;
                                          Error     : Ada.Exceptions.Exception_Occurrence);

end Asynchronous.Utilities.Debug;
