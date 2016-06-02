with Events.Interfaces;
with Events.Servers;
with Utilities.References.Not_Null;
pragma Elaborate_All (Utilities.References.Not_Null);

package Events.Composition.And_Gates is

   -- This is an implementation of AND gates as defined in the parent package
   type And_Gate is private;

   -- Composite events are created by composing multiple events with one another
   procedure Add_Child (Where : in out And_Gate;
                        Who   : in out Event_Client);

   -- Children may be added in a bulk fashion for increased efficiency
   procedure Add_Children (Where : in out And_Gate;
                           Who   : in out Event_List);

   -- Composite events produce an event which is semantically equivalent to the sum of its parts.
   -- After producing such a client, the composite event is considered to be frozen, in the sense that it is a run-time
   -- error to attempt to add more clients to it.
   function Make_Client (From : And_Gate) return Event_Client;
   Composite_Event_Already_Frozen : exception;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   subtype Event_Server is Servers.Server;

   protected type And_Gate_Implementation is
      procedure Notify_Event_Status_Change (What : Interfaces.Finished_Event_Status);
      procedure Add_Children (Count : Natural);
      procedure Make_Client (Where : out Event_Client);
   private
      Is_Frozen : Boolean := False;
      Child_Count : Natural := 0;
      Done_Children : Natural := 0;
      Current_Status : Interfaces.Event_Status := Interfaces.Pending;
      Event : Event_Server;
      procedure Propagate_Status_Change;
   end And_Gate_Implementation;

   package And_Gate_Reference_Base is new Utilities.References (And_Gate_Implementation);
   package And_Gate_References is new And_Gate_Reference_Base.Not_Null;

   type And_Gate is new Interfaces.Event_Listener_Reference with
      record
         Ref : And_Gate_References.Reference;
      end record;
   overriding procedure Notify_Event_Status_Change (Where : in out And_Gate;
                                                    What  : Interfaces.Finished_Event_Status);

end Events.Composition.And_Gates;
