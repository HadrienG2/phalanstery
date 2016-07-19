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

with Phalanstery.Outcomes.Interfaces;

package Phalanstery.Outcomes.Callbacks is

   -- For situations where full-blown outcome listener objects are not needed, we provide a simple listener object
   -- wrapping any global callback function.

   type Outcome_Callback is not null access procedure (What : Interfaces.Final_Outcome_Status);

   type Callback_Listener (<>) is new Interfaces.Outcome_Listener_Reference with private;

   not overriding function Make_Callback_Listener (From : Outcome_Callback) return Callback_Listener;

   overriding procedure Notify_Outcome (Where : in out Callback_Listener;
                                        What  : Interfaces.Final_Outcome_Status);

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Callback_Listener is new Interfaces.Outcome_Listener_Reference with
      record
         Callback : Outcome_Callback;
      end record;

end Phalanstery.Outcomes.Callbacks;
