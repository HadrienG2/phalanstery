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

package body Phalanstery.Events.Callbacks is

   not overriding function Make_Callback_Listener (From : Event_Status_Callback) return Callback_Listener is
     ((Callback => From));

   overriding procedure Notify_Event_Status_Change (Where : in out Callback_Listener;
                                                    What  : Interfaces.Finished_Event_Status) is
   begin
      Where.Callback.all (What);
   end Notify_Event_Status_Change;


   -- The remainder of this package is dedicated to unit tests
   Test_Callback_Calls : Natural := 0;
   Last_Status : Interfaces.Finished_Event_Status;

   procedure Test_Callback (Final_Status : Interfaces.Finished_Event_Status) is
   begin
      Test_Callback_Calls := Test_Callback_Calls + 1;
      Last_Status := Final_Status;
   end Test_Callback;

   procedure Run_Tests is
      use Utilities.Testing;
      use all type Interfaces.Event_Status;
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

end Phalanstery.Events.Callbacks;
