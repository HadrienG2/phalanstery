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

with Ada.Exceptions;
with Phalanstery.Events.Implementation;
with Phalanstery.Events.Interfaces;

package Phalanstery.Events.Clients is

   -- This is an implementation of event clients. You should never attempt to create clients directly,
   -- but instead get event clients from an event server as appropriate.

   type Client is new Interfaces.Event_Client with private;

   overriding function Is_Null (Who : Client) return Boolean;

   overriding function "=" (A, B : Client) return Boolean;

   overriding function Status (Who : Client) return Interfaces.Event_Status;

   overriding procedure Get_Error (Who  : Client;
                                   What : out Ada.Exceptions.Exception_Occurrence);

   overriding procedure Wait_Completion (Who : Client);

   overriding procedure Add_Listener (Where : in out Client;
                                      Who   : in out Interfaces.Event_Listener_Reference'Class);

   overriding procedure Cancel (Who : in out Client);

   -- This function is needed in order to allow generating event clients from event servers
   not overriding function Make_Client (Event : Implementation.Event_Reference) return Client
     with Pre => (not Event.Is_Null),
          Post => (not Make_Client'Result.Is_Null);

   -- NOTE : Because server and client operation is intertwined, unit tests for clients are in the Servers package

private

   type Client is new Interfaces.Event_Client with
      record
         Ref : Implementation.Event_Reference;
      end record;

end Phalanstery.Events.Clients;
