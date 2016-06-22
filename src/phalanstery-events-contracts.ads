with Phalanstery.Events.Clients;
with Phalanstery.Events.Servers;

package Phalanstery.Events.Contracts is

   -- Although allowing events handles to be null is great for performance, it is not generally desirable.
   -- We thus propose subtypes of event handles which are guaranteed by contract programming not to be null.

   subtype Valid_Event_Client is Events.Clients.Client
     with Dynamic_Predicate => (not Valid_Event_Client.Is_Null);

   subtype Valid_Event_Server is Events.Servers.Server
     with Dynamic_Predicate => (not Valid_Event_Server.Is_Null);

end Phalanstery.Events.Contracts;
