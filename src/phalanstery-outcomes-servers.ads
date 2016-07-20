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
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Implementation;
with Phalanstery.Outcomes.Interfaces;

package Phalanstery.Outcomes.Servers is

   -- This is an implementation of outcome object servers. Creating an outcome server silently spawns an outcome object
   -- under the hood. The lifetime of this object is subsequently managed through counting of servers and clients.

   -- Outcome servers implement the server interface defined in Outcomes.Interfaces
   type Server is limited new Interfaces.Outcome_Server with private;

   overriding function Is_Null (Who : Server) return Boolean;

   overriding function "=" (A, B : Server) return Boolean;

   overriding function Make_Outcome return Server;

   overriding procedure Mark_Done (Who : in out Server);

   overriding procedure Mark_Error (Who  : in out Server;
                                    What : Ada.Exceptions.Exception_Occurrence);

   overriding procedure Cancel (Who : in out Server);

   overriding function Is_Canceled (Who : Server) return Boolean;

   -- This method cannot be part of the Outcome_Server interface because Ada does not support multiple dispatching.
   -- However, it is necessary to the proper operation of outcome objects, since it is the way one creates outcome
   -- clients from outcome servers.
   not overriding function Make_Client (From : Server) return Clients.Client
     with Pre => (not From.Is_Null),
          Post => (not Make_Client'Result.Is_Null);

   -- Run unit tests for BOTH the Clients and Servers packages
   procedure Run_Tests;

private

   type Server is limited new Interfaces.Outcome_Server with
      record
         Ref : Implementation.Outcome_Reference;
      end record;

end Phalanstery.Outcomes.Servers;
