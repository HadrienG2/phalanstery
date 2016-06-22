with Phalanstery.Events.Interfaces;

package Phalanstery.Events.Callbacks is

   -- As an example of an event listener object, we support a thin wrapper for global callbacks.

   type Event_Status_Callback is not null access procedure (What : Interfaces.Finished_Event_Status);

   type Callback_Listener (<>) is new Interfaces.Event_Listener_Reference with private;

   not overriding function Make_Callback_Listener (From : Event_Status_Callback) return Callback_Listener;

   overriding procedure Notify_Event_Status_Change (Where : in out Callback_Listener;
                                                    What  : Interfaces.Finished_Event_Status);

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Callback_Listener is new Interfaces.Event_Listener_Reference with
      record
         Callback : Event_Status_Callback;
      end record;

end Phalanstery.Events.Callbacks;
