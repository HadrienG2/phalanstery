with Ada.Exceptions;
with Phalanstery.Events.Clients;
with Phalanstery.Events.Implementation;
with Phalanstery.Events.Interfaces;

package Phalanstery.Events.Servers is

   -- This is a reference-counted implementation of event servers
   type Server is limited new Interfaces.Event_Server with private;

   overriding function Is_Null (Who : Server) return Boolean;

   overriding function "=" (A, B : Server) return Boolean;

   overriding procedure Mark_Done (Who : in out Server);

   overriding procedure Mark_Error (Who  : in out Server;
                                    What : Ada.Exceptions.Exception_Occurrence);

   overriding procedure Cancel (Who : in out Server);

   overriding function Is_Canceled (Who : Server) return Boolean;

   -- This cannot be made part of the Event_Server interface, but is required for event completeness.
   not overriding function Make_Event return Server
     with Post => (not Make_Event'Result.Is_Null);

   not overriding function Make_Client (From : Server) return Clients.Client
     with Pre => (not From.Is_Null),
          Post => (not Make_Client'Result.Is_Null);

   -- Run unit tests for BOTH the Clients and Servers packages
   procedure Run_Tests;

private

   type Server is limited new Interfaces.Event_Server with
      record
         Ref : Implementation.Event_Reference;
      end record;

end Phalanstery.Events.Servers;
