with Ada.Exceptions;
with Events.Clients;
with Events.Implementation;
with Events.Interfaces; use Events.Interfaces;

package Events.Servers is

   -- This is a reference-counted implementation of event servers
   type Server is limited new Event_Server with private;

   overriding function "=" (A, B : Server) return Boolean;

   overriding procedure Mark_Done (Who : in out Server);

   overriding procedure Mark_Error (Who  : in out Server;
                                    What : Ada.Exceptions.Exception_Occurrence);

   overriding procedure Cancel (Who : in out Server);

   overriding function Is_Canceled (Who : Server) return Boolean;

   -- This function cannot be made part of the Event_Server interface, but is required for event completeness.
   not overriding function Make_Client (From : Server) return Clients.Client;

   -- Run unit tests for BOTH the Clients and Servers packages
   procedure Run_Tests;

private

   subtype Event_Reference is Implementation.References.Reference;

   type Server is limited new Event_Server with
      record
         Ref : Event_Reference;
      end record;

end Events.Servers;
