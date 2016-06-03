with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Utilities.Signals is

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

end Asynchronous.Utilities.Signals;
