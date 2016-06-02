with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Events.Interfaces; use Events.Interfaces;
with Utilities.References;
with Utilities.References.Nullable;
pragma Elaborate_All (Utilities.References.Nullable);

package Events.Implementation is

   -- This package features an implementation of event objects as described by Events.Interfaces.
   -- It does not implement events clients and servers directly, for that see Events.Clients and Events.Servers.

   package Event_Listener_Lists is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Event_Listener_Reference'Class);

   subtype Event_Listener_List is Event_Listener_Lists.Vector;

   protected type Event is

      function Status return Event_Status;
      procedure Get_Error (What : out Ada.Exceptions.Exception_Occurrence);

      entry Wait_Completion (Final_Status : out Event_Status);

      procedure Add_Listener (Who : in out Event_Listener_Reference'Class);

      procedure Cancel;
      function Is_Canceled return Boolean;

      procedure Mark_Done;
      procedure Mark_Error (What : Ada.Exceptions.Exception_Occurrence);

   private

      Current_Status : Event_Status := Pending;
      Event_Error : Ada.Exceptions.Exception_Occurrence;
      Listeners : Event_Listener_List;

      procedure Notify_Status_Change;

   end Event;

   package Event_Reference_Base is new Utilities.References (Events.Implementation.Event);
   package Event_References is new Event_Reference_Base.Nullable;
   subtype Event_Reference is Event_References.Reference;
   function Make_Event return Event_Reference renames Event_References.Make;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Events.Implementation;
