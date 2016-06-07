with Ada.Exceptions;
with Asynchronous.Events.Implementation;
with Asynchronous.Events.Interfaces;

package Asynchronous.Events.Clients is

   -- This is an implementation of event clients. You should never need to create clients directly,
   -- but instead get event clients from an event server as appropriate.

   type Client is new Interfaces.Event_Client with private;

   overriding function Is_Null (Who : Client) return Boolean;

   overriding function "=" (A, B : Client) return Boolean;

   overriding function Status (Who : Client) return Interfaces.Event_Status;

   overriding procedure Get_Error (Who  : Client;
                                   What : out Ada.Exceptions.Exception_Occurrence);

   overriding procedure Wait_Completion (Who          : Client;
                                         Final_Status : out Interfaces.Finished_Event_Status);

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

end Asynchronous.Events.Clients;
