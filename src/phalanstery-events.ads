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

package Phalanstery.Events with Pure is

   -- This package, together with its children, provides an implementation of a job synchronization primitive called
   -- "event", which is a state machine representing the underlying progress of some asynchronous operation.
   --
   -- There are currently four defined event states :
   --    - An event is created in the "Pending" state.
   --    - If the underlying operation runs to completion, the associated event will swith to the "Done" state.
   --    - If an attempt is made to cancel a pending operation, the event will switch to the "Canceled" state.
   --    - If the underlying operation aborts with an exception, the event will switch to the "Error" state and store
   --      additional information about the nature of the operation that has occured.
   --
   -- Event states may be synchronously queried in various ways, from polling to blocking. However, the preferred way
   -- to monitor events is using user-defined callback objects called listeners.
   --
   -- The event implementation is decomposed into child packages as follows:
   --    - Events.Interfaces defines the basic client/server interface followed by events.
   --    - Events.Implementation implements event synchronization using a reference-counted protected object.
   --    - Events.Clients and Events.Servers finish the implementation of the event interface on top of these handles.
   --
   -- In addition, the following event-related convenience packages are provided:
   --    - Events.Callbacks provides the simplest possible example of an event listener : a global callback procedure.
   --    - Events.Composition provides a way to compose events objects using an AND relationship.

end Phalanstery.Events;
