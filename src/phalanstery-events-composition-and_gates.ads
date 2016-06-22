with Phalanstery.Events.Interfaces;
with Phalanstery.Events.Servers;
with Phalanstery.Utilities.References.Not_Null;
pragma Elaborate_All (Phalanstery.Utilities.References.Not_Null);

package Phalanstery.Events.Composition.And_Gates is

   -- This is an implementation of AND gates as defined in the parent package
   type And_Gate is private;

   -- Composite events are created by composing multiple events with one another
   procedure Add_Child (Where : in out And_Gate;
                        Who   : in out Valid_Event_Client)
     with Pre => (not Is_Frozen (Where));

   -- Children may be added in a bulk fashion for increased efficiency
   procedure Add_Children (Where : in out And_Gate;
                           Who   : in out Valid_Event_List)
     with Pre => (not Is_Frozen (Where));

   -- At some point, one may produce a client event which is equivalent to the AND-sum of the children of the gate
   function Make_Client (From : in out And_Gate) return Valid_Event_Client
     with Post => (Is_Frozen (From));

   -- After producing such a client, the composite event is considered to be frozen, in the sense that it is a run-time
   -- error to attempt to add more clients to it.
   function Is_Frozen (What : And_Gate) return Boolean;
   Composite_Event_Already_Frozen : exception;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   use all type Interfaces.Event_Status;

   protected type And_Gate_Implementation is
      procedure Notify_Event_Status_Change (What : Interfaces.Finished_Event_Status);
      procedure Add_Children (Count : Natural);
      procedure Make_Client (Where : out Valid_Event_Client);
      function Is_Frozen return Boolean;
   private
      Frozen : Boolean := False;
      Child_Count : Natural := 0;
      Done_Children : Natural := 0;
      Current_Status : Interfaces.Event_Status := Pending;
      Event : Valid_Event_Server := Servers.Make_Event;
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

end Phalanstery.Events.Composition.And_Gates;
