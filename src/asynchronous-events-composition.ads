with Asynchronous.Events.Clients;
with Asynchronous.Events.Servers;

package Asynchronous.Events.Composition is

   -- First, let us define some convenience notations
   subtype Event_Client is Clients.Client;
   subtype Event_Server is Servers.Server;
   type Event_List is array (Positive range <>) of Event_Client;

   -- Events, as an asynchronous abstraction should be composable. For now, we only support AND-gate-like composition.
   -- The design of Ada protected types makes it somewhat hard to support other kinds of composition, but if a clear
   -- need for them emerges, it can nevertheless be implemented in the future.
   --
   -- The rules for AND-gate composition are the following :
   --    - An AND gate with zero children is Done
   --    - If any child is Pending, the AND gate is Pending
   --    - If all children are Done, the AND gate is Done
   --    - If any child is Canceled, the AND gate is Canceled
   --    - If any child is Error, the AND gate is Error with the special exception Child_Error
   --
   Child_Error : exception;

   -- The children of this package are organized as follows:
   --    - Composition.And_Gates presents a raw AND gate abstraction.
   --    - Composition.Shortcuts presents convenience shortcuts for event composition.

end Asynchronous.Events.Composition;
