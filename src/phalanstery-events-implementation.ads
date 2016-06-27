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

with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Phalanstery.Events.Interfaces;
with Phalanstery.Utilities.References;
with Phalanstery.Utilities.References.Nullable;
pragma Elaborate_All (Phalanstery.Utilities.References.Nullable);

package Phalanstery.Events.Implementation is

   -- This package features an implementation of event objects as described by Events.Interfaces.
   -- It does not implement events clients and servers directly, for that see Events.Clients and Events.Servers.

   use type Interfaces.Event_Listener_Reference;

   package Event_Listener_Lists is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Interfaces.Event_Listener_Reference'Class);

   subtype Event_Listener_List is Event_Listener_Lists.Vector;

   protected type Event is

      function Status return Interfaces.Event_Status;
      procedure Get_Error (What : out Ada.Exceptions.Exception_Occurrence);

      entry Wait_Completion;

      procedure Add_Listener (Who : in out Interfaces.Event_Listener_Reference'Class);

      procedure Cancel;
      function Is_Canceled return Boolean;

      procedure Mark_Done;
      procedure Mark_Error (What : Ada.Exceptions.Exception_Occurrence);

   private

      Current_Status : Interfaces.Event_Status := Interfaces.Pending;
      Event_Error : Ada.Exceptions.Exception_Occurrence;
      Listeners : Event_Listener_List;

      procedure Notify_Status_Change;

   end Event;

   package Event_Reference_Base is new Utilities.References (Event);
   package Event_References is new Event_Reference_Base.Nullable;
   subtype Event_Reference is Event_References.Reference;
   function Make_Event return Event_Reference renames Event_References.Make;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Events.Implementation;
