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

with Phalanstery.Events.Interfaces;

package Phalanstery.Events.Callbacks is

   -- As an example of an event listener object, we support a thin wrapper for global callbacks.

   type Event_Status_Callback is not null access procedure (What : Interfaces.Finished_Event_Status);

   type Callback_Listener (<>) is new Interfaces.Event_Listener_Reference with private;

   not overriding function Make_Callback_Listener (From : Event_Status_Callback) return Callback_Listener;

   overriding procedure Notify_Event_Status_Change (Where : in out Callback_Listener;
                                                    What  : Interfaces.Finished_Event_Status);

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Callback_Listener is new Interfaces.Event_Listener_Reference with
      record
         Callback : Event_Status_Callback;
      end record;

end Phalanstery.Events.Callbacks;
