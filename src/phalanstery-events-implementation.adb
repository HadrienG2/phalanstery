with Ada.Assertions;
with Phalanstery.Events.Callbacks;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Events.Implementation is

   use all type Interfaces.Event_Status;

   protected body Event is

      function Status return Interfaces.Event_Status is (Current_Status);

      procedure Get_Error (What : out Ada.Exceptions.Exception_Occurrence) is
      begin
         Ada.Exceptions.Save_Occurrence (Target => What,
                                         Source => Event_Error);
      end Get_Error;

      entry Wait_Completion when Status /= Pending is
      begin
         case Current_Status is
            when Pending =>
               raise Ada.Assertions.Assertion_Error;  -- Should never happen
            when Done =>
               return;
            when Error =>
               Ada.Exceptions.Reraise_Occurrence (Event_Error);
            when Canceled =>
               raise Interfaces.Event_Canceled;
         end case;
      end Wait_Completion;

      procedure Add_Listener (Who : in out Interfaces.Event_Listener_Reference'Class) is
      begin
         if Current_Status = Pending then
            Listeners.Append (Who);
         else
            Who.Notify_Event_Status_Change (Current_Status);
         end if;
      end Add_Listener;

      procedure Cancel is
      begin
         if Current_Status = Pending then
            Current_Status := Canceled;
            Notify_Status_Change;
         end if;
      end Cancel;

      function Is_Canceled return Boolean is (Current_Status = Canceled);

      procedure Mark_Done is
      begin
         if Current_Status = Pending then
            Current_Status := Done;
            Notify_Status_Change;
         end if;
      end Mark_Done;

      procedure Mark_Error (What : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Current_Status = Pending then
            Current_Status := Error;
            Ada.Exceptions.Save_Occurrence (Target => Event_Error,
                                            Source => What);
            Notify_Status_Change;
         end if;
      end Mark_Error;

      procedure Notify_Status_Change is
      begin
         for Listener of Listeners loop
            Listener.Notify_Event_Status_Change (Current_Status);
         end loop;
         Listeners.Clear;
      end Notify_Status_Change;

   end Event;


   -- The remainder of this package is dedicated to unit tests
   Test_Callback_Calls : Natural := 0;
   Last_Status : Interfaces.Finished_Event_Status;

   procedure Test_Callback (Final_Status : Interfaces.Finished_Event_Status) is
   begin
      Test_Callback_Calls := Test_Callback_Calls + 1;
      Last_Status := Final_Status;
   end Test_Callback;

   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Exceptions.Exception_Id;

      Test_Error : Ada.Exceptions.Exception_Occurrence;
      Custom_Error : exception;
      Custom_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

      -- Final_Status : Interfaces.Event_Status;
      Test_Callback_Listener : Callbacks.Callback_Listener := Callbacks.Make_Callback_Listener (Test_Callback'Access);

      procedure Setup_Tests is
      begin
         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurence);
      end Setup_Tests;

      procedure Test_Initial_State is
         Test_Event : Event;
      begin
         Assert_Truth (Check   => (Test_Event.Status = Pending),
                       Message => "Initial event status should be Pending");

         Test_Event.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Freshly initialized events should be devoid of exceptions");

         select
            Test_Event.Wait_Completion;
            Fail ("Pending events should lead to blocking waits");
         else
            null;
         end select;

         Test_Event.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 0),
                       Message => "Event callbacks should not be called upon binding to a newly created event");

         Assert_Truth (Check   => not Test_Event.Is_Canceled,
                       Message => "Freshly initialized events should not be Canceled");
      end Test_Initial_State;

      procedure Test_Done_State is
         Test_Event : Event;
      begin
         Test_Event.Add_Listener (Test_Callback_Listener);
         Test_Event.Mark_Done;
         Assert_Truth (Check   => (Test_Event.Status = Done),
                       Message => "Upon calling Mark_Done, events should switch to the Done status");

         Test_Event.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Normally completed events should be devoid of exceptions");

         select
            Test_Event.Wait_Completion;
         else
            Fail ("Waiting for a done event should not block");
         end select;

         Assert_Truth (Check   => (Test_Callback_Calls = 1) and (Last_Status = Done),
                       Message => "Previously set callbacks should be fired when the event changes state");

         Test_Event.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 2) and (Last_Status = Done),
                       Message => "Callbacks which are set after event completion should be fired immediately");

         Assert_Truth (Check   => not Test_Event.Is_Canceled,
                       Message => "Done events should not be marked as canceled");

         Test_Event.Mark_Done;
         Assert_Truth (Check   => (Test_Event.Status = Done) and (Test_Callback_Calls = 2),
                       Message => "Marking a done event as done again should have no effect");

         Test_Event.Cancel;
         Assert_Truth (Check   => (Test_Event.Status = Done) and (not Test_Event.Is_Canceled),
                       Message => "Attempting to cancel an event which is already done should have no effect");

         Test_Event.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (Test_Event.Status = Done),
                       Message => "Attempting to insert an error into a done event should have no effect");

         Test_Event.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Errors inserted into a done event should not be visible");
      end Test_Done_State;

      procedure Test_Canceled_State is
         Test_Event : Event;
      begin
         Test_Event.Add_Listener (Test_Callback_Listener);
         Test_Event.Cancel;
         Assert_Truth (Check   => (Test_Event.Status = Canceled),
                       Message => "Canceling a pending event should lead it to the Canceled state");

         Test_Event.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Canceled events should be devoid of exceptions");

         begin
            select
               Test_Event.Wait_Completion;
               Fail ("Waiting for a canceled event should raise Event_Canceled");
            else
               Fail ("Waiting for a canceled event should not block");
            end select;
         exception
            when Interfaces.Event_Canceled =>
               null;
         end;

         Assert_Truth (Check   => (Test_Callback_Calls = 3) and (Last_Status = Canceled),
                       Message => "Previously set callbacks should be fired when the event is canceled");

         Test_Event.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 4) and (Last_Status = Canceled),
                       Message => "Callbacks which are set after event cancelation should be fired immediately");

         Assert_Truth (Check   => Test_Event.Is_Canceled,
                       Message => "Canceled events should be marked as such");

         Test_Event.Cancel;
         Assert_Truth (Check   => (Test_Event.Status = Canceled) and (Test_Callback_Calls = 4),
                       Message => "Canceling an already canceled event should have no effect");

         Test_Event.Mark_Done;
         Assert_Truth (Check   => (Test_Event.Status = Canceled) and (Test_Callback_Calls = 4),
                       Message => "Marking a canceled event as done should have no effect");

         Test_Event.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (Test_Event.Status = Canceled),
                       Message => "Attempting to insert an error into a canceled event should have no effect");

         Test_Event.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Errors inserted into a canceled event should not be visible");
      end Test_Canceled_State;

      procedure Test_Error_State is
         Test_Event : Event;
      begin
         Test_Event.Add_Listener (Test_Callback_Listener);
         Test_Event.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (Test_Event.Status = Error),
                       Message => "Marking an event as erronerous should lead it to the Error state");

         Test_Event.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Custom_Error'Identity),
                       Message => "Errors should be properly reported to the curious reader");

         begin
            select
               Test_Event.Wait_Completion;
               Fail ("Waiting for an erronerous event should re-raise the underlying exception");
            else
               Fail ("Waiting for an erronerous event should not block");
            end select;
         exception
            when Custom_Error =>
               null;
         end;

         Assert_Truth (Check   => (Test_Callback_Calls = 5) and (Last_Status = Error),
                       Message => "Previously set callbacks should be fired when the event fails");

         Test_Event.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 6) and (Last_Status = Error),
                       Message => "Callbacks which are set after event failure should be fired immediately");

         Assert_Truth (Check   => not Test_Event.Is_Canceled,
                       Message => "Erronerous events should not be marked as canceled");

         Test_Event.Cancel;
         Assert_Truth (Check   => (Test_Event.Status = Error) and (Test_Callback_Calls = 6),
                       Message => "Canceling an erronerous event should have no effect");

         Test_Event.Mark_Done;
         Assert_Truth (Check   => (Test_Event.Status = Error) and (Test_Callback_Calls = 6),
                       Message => "Marking an erronerous event as done should have no effect");
      end Test_Error_State;

   begin
      Setup_Tests;
      Test_Initial_State;
      Test_Done_State;
      Test_Canceled_State;
      Test_Error_State;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Events.Implementation;
