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
