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
with Phalanstery.Outcomes.Implementation;
with Phalanstery.Outcomes.Interfaces;

package Phalanstery.Outcomes.Clients is

   -- This is an implementation of outcome object clients. You should never attempt to create these clients directly,
   -- but instead spawn them from an outcome server using its Make_Client method.

   -- Outcome clients implement the client interface defined in Outcomes.Interfaces
   type Client is new Interfaces.Outcome_Client with private;

   overriding function Is_Null (Who : Client) return Boolean;

   overriding function "=" (A, B : Client) return Boolean;

   overriding function Status (Who : Client) return Interfaces.Outcome_Status;

   overriding procedure Get_Error (Who  : Client;
                                   What : out Ada.Exceptions.Exception_Occurrence);

   overriding procedure Wait_Completion (Who : Client);

   overriding procedure Add_Listener (Where : in out Client;
                                      Who   : in out Interfaces.Outcome_Listener_Reference'Class);

   overriding procedure Cancel (Who : in out Client);

   -- This function is an implementation detail. It cannot be private because the Outcomes.Servers package needs it in
   -- order to spawn the clients associated with an outcome server object. However, one should never use it directly.
   not overriding function Make_Client (From : Implementation.Outcome_Reference) return Client
     with Pre  => (not From.Is_Null),
          Post => (not Make_Client'Result.Is_Null);

   -- NOTE : Since outcome clients and servers are only usable together as a communication channel, they should be
   --        tested together. The unit tests for outcome clients are thus locasted in Outcomes.Servers.

private

   type Client is new Interfaces.Outcome_Client with
      record
         Ref : Implementation.Outcome_Reference;
      end record;

end Phalanstery.Outcomes.Clients;
