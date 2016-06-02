package Events.Composition.Shortcuts is

   -- In the simplest use cases, the whole AND gate life cycle may be shortened into the following convenience function.
   -- For more complicated use cases, see the And_Gates child package for raw access to the AND gate implementation
   function When_All (Wait_List : Event_List) return Event_Client;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Events.Composition.Shortcuts;
