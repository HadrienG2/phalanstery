with Utilities.Testing;
pragma Elaborate_All (Utilities.Testing);

package body Events.Callbacks is

   not overriding function Make_Callback_Listener (From : Event_Status_Callback) return Callback_Listener is
     ((Callback => From));

   overriding procedure Notify_Event_Status_Change (Where : in out Callback_Listener;
                                                    What  : Finished_Event_Status) is
   begin
      Where.Callback.all (What);
   end Notify_Event_Status_Change;


   -- The remainder of this package is dedicated to unit tests
   Test_Callback_Calls : Natural := 0;
   Last_Status : Event_Status;

   procedure Test_Callback (Final_Status : Event_Status) is
   begin
      Test_Callback_Calls := Test_Callback_Calls + 1;
      Last_Status := Final_Status;
   end Test_Callback;

   procedure Run_Tests is
      use Utilities.Testing;
      Test_Callback_Listener : Callback_Listener := Make_Callback_Listener (Test_Callback'Access);
   begin
      Test_Callback_Listener.Notify_Event_Status_Change (Done);
      Assert_Truth (Check   => (Test_Callback_Calls = 1) and (Last_Status = Done),
                    Message => "Callbacks should be fired when the corresponding wrapper object is notified");

      Test_Callback_Listener.Notify_Event_Status_Change (Error);
      Assert_Truth (Check   => (Test_Callback_Calls = 2) and (Last_Status = Error),
                    Message => "Callback listeners should respond correctly to multiple calls");
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Events.Callbacks;
