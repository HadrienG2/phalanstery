with Ada.Exceptions;
with Events.Implementation;
with Events.Interfaces; use Events.Interfaces;

package Events.Clients is

   -- This is an implementation of event clients. You should never need to create clients directly,
   -- but instead get event clients from an event server as appropriate.

   type Client is new Event_Client with private;

   overriding function "=" (A, B : Client) return Boolean;

   overriding function Status (Who : Client) return Event_Status;

   overriding procedure Get_Error (Who  : Client;
                                   What : out Ada.Exceptions.Exception_Occurrence);

   overriding procedure Wait_Completion (Who          : Client;
                                         Final_Status : out Finished_Event_Status);

   overriding procedure Add_Listener (Where : in out Client;
                                      Who   : in out Event_Listener_Reference'Class);

   overriding procedure Cancel (Who : in out Client);

   -- This function is needed in order to allow generating event clients from event servers
   subtype Event_Reference is Implementation.References.Reference;
   not overriding function Make_Client (Event : Event_Reference) return Client;

   -- NOTE : Because server and client operation is intertwined, unit tests are located in the Servers package

private

   type Client is new Event_Client with
      record
         Ref : Event_Reference;
      end record;

end Events.Clients;
