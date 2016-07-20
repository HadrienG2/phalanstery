-- Copyright 2016 Hadrien Grasland
--
-- This file is part of Phalanstery.
--
-- Phalanstery is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Phalanstery is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Phalanstery.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Assertions;
with Phalanstery.Outcomes.Callbacks;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Outcomes.Implementation is

   use type Interfaces.Outcome_Listener_Reference;
   use all type Interfaces.Outcome_Status;

   protected body Outcome is

      function Status return Interfaces.Outcome_Status is (Current_Status);

      procedure Get_Error (What : out Ada.Exceptions.Exception_Occurrence) is
      begin
         Ada.Exceptions.Save_Occurrence (Target => What,
                                         Source => Current_Error);
      end Get_Error;

      entry Wait_Completion when Status /= Pending is
      begin
         case Current_Status is
            when Pending =>
               raise Ada.Assertions.Assertion_Error;  -- Should never happen
            when Done =>
               return;
            when Error =>
               Ada.Exceptions.Reraise_Occurrence (Current_Error);
            when Canceled =>
               raise Interfaces.Operation_Canceled;
         end case;
      end Wait_Completion;

      procedure Add_Listener (Who : in out Interfaces.Outcome_Listener_Reference'Class) is
      begin
         if Current_Status = Pending then
            Listeners.Append (Who);
         else
            Who.Notify_Outcome (Current_Status);
         end if;
      end Add_Listener;

      procedure Cancel is
      begin
         if Current_Status = Pending then
            Current_Status := Canceled;
            Notify_Listeners;
         end if;
      end Cancel;

      function Is_Canceled return Boolean is (Current_Status = Canceled);

      procedure Mark_Done is
      begin
         if Current_Status = Pending then
            Current_Status := Done;
            Notify_Listeners;
         end if;
      end Mark_Done;

      procedure Mark_Error (What : Ada.Exceptions.Exception_Occurrence) is
      begin
         if Current_Status = Pending then
            Current_Status := Error;
            Ada.Exceptions.Save_Occurrence (Target => Current_Error,
                                            Source => What);
            Notify_Listeners;
         end if;
      end Mark_Error;

      procedure Notify_Listeners is
      begin
         for Listener of Listeners loop
            Listener.Notify_Outcome (Current_Status);
         end loop;
         Listeners.Clear;
      end Notify_Listeners;

   end Outcome;


   -- The remainder of this package is dedicated to unit tests
   Test_Callback_Calls : Natural := 0;
   Last_Status : Interfaces.Final_Outcome_Status;

   procedure Test_Callback (Final_Status : Interfaces.Final_Outcome_Status) is
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

      Test_Callback_Listener : Callbacks.Callback_Listener := Callbacks.Make_Callback_Listener (Test_Callback'Access);

      procedure Setup_Tests is
      begin
         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurence);
      end Setup_Tests;

      procedure Test_Pending_State is
         Test_Outcome : Outcome;
      begin
         Assert_Truth (Check   => (Test_Outcome.Status = Pending),
                       Message => "Initial outcome status should be Pending");

         Test_Outcome.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Freshly initialized outcomes should be devoid of exceptions");

         select
            Test_Outcome.Wait_Completion;
            Fail ("Waits for pending outcomes should be blocking");
         else
            null;
         end select;

         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 0),
                       Message => "Outcome callbacks should not be called by a pending outcome object");

         Assert_Truth (Check   => not Test_Outcome.Is_Canceled,
                       Message => "Freshly initialized outcome objects should not be Canceled");
      end Test_Pending_State;

      procedure Test_Done_State is
         Test_Outcome : Outcome;
      begin
         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Test_Outcome.Mark_Done;
         Assert_Truth (Check   => (Test_Outcome.Status = Done),
                       Message => "Upon calling Mark_Done, outcome objects should switch to the Done status");

         Test_Outcome.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "No exception should be associated with an operation that completed normally");

         select
            Test_Outcome.Wait_Completion;
         else
            Fail ("Waiting for a done outcome object should not block");
         end select;

         Assert_Truth (Check   => (Test_Callback_Calls = 1) and (Last_Status = Done),
                       Message => "Previously set callbacks should be fired when the outcome object changes state");

         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 2) and (Last_Status = Done),
                       Message => "Callbacks which are set after operation completion should be fired immediately");

         Assert_Truth (Check   => not Test_Outcome.Is_Canceled,
                       Message => "Outcome objects associated with completed operations should not be Canceled");

         Test_Outcome.Mark_Done;
         Assert_Truth (Check   => (Test_Outcome.Status = Done) and (Test_Callback_Calls = 2),
                       Message => "Marking a done outcome object as done again should have no effect");

         Test_Outcome.Cancel;
         Assert_Truth (Check   => (Test_Outcome.Status = Done) and (not Test_Outcome.Is_Canceled),
                       Message => "Attempting to cancel an operation which is already done should have no effect");

         Test_Outcome.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (Test_Outcome.Status = Done),
                       Message => "Attempting to insert an error in a done outcome object should not alter its status");

         Test_Outcome.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Errors inserted into a done outcome object should not be visible");
      end Test_Done_State;

      procedure Test_Canceled_State is
         Test_Outcome : Outcome;
      begin
         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Test_Outcome.Cancel;
         Assert_Truth (Check   => (Test_Outcome.Status = Canceled),
                       Message => "Canceling a pending outcome object should lead it to the Canceled state");

         Test_Outcome.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Canceled outcome objects should be devoid of exceptions");

         begin
            select
               Test_Outcome.Wait_Completion;
               Fail ("Waiting for a canceled outcome object should raise Operation_Canceled");
            else
               Fail ("Waiting for a canceled outcome object should not block");
            end select;
         exception
            when Interfaces.Operation_Canceled =>
               null;
         end;

         Assert_Truth (Check   => (Test_Callback_Calls = 3) and (Last_Status = Canceled),
                       Message => "Previously set callbacks should be fired when an outcome object is canceled");

         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 4) and (Last_Status = Canceled),
                       Message => "Callbacks set after outcome object cancelation should be fired immediately");

         Assert_Truth (Check   => Test_Outcome.Is_Canceled,
                       Message => "Canceled outcome objects should be marked as such");

         Test_Outcome.Cancel;
         Assert_Truth (Check   => (Test_Outcome.Status = Canceled) and (Test_Callback_Calls = 4),
                       Message => "Canceling a canceled outcome object should have no effect");

         Test_Outcome.Mark_Done;
         Assert_Truth (Check   => (Test_Outcome.Status = Canceled) and (Test_Callback_Calls = 4),
                       Message => "Marking a canceled outcome object as done should have no effect");

         Test_Outcome.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (Test_Outcome.Status = Canceled),
                       Message => "Attempting to insert an error into a canceled outcome object should have no effect");

         Test_Outcome.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Errors inserted into a canceled outcome object should not be visible");
      end Test_Canceled_State;

      procedure Test_Error_State is
         Test_Outcome : Outcome;
      begin
         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Test_Outcome.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (Test_Outcome.Status = Error),
                       Message => "Marking an outcome object as erronerous should lead it to the Error state");

         Test_Outcome.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Custom_Error'Identity),
                       Message => "Errors should be properly reported to the curious reader");

         begin
            select
               Test_Outcome.Wait_Completion;
               Fail ("Waiting for an erronerous outcome object should re-raise the underlying exception");
            else
               Fail ("Waiting for an erronerous outcome object should not block");
            end select;
         exception
            when Custom_Error =>
               null;
         end;

         Assert_Truth (Check   => (Test_Callback_Calls = 5) and (Last_Status = Error),
                       Message => "Previously set callbacks should be fired when the associated operation fails");

         Test_Outcome.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 6) and (Last_Status = Error),
                       Message => "Callbacks which are set after operation failure should be fired immediately");

         Assert_Truth (Check   => not Test_Outcome.Is_Canceled,
                       Message => "Erronerous outcome objects should not be marked as canceled");

         Test_Outcome.Cancel;
         Assert_Truth (Check   => (Test_Outcome.Status = Error) and (Test_Callback_Calls = 6),
                       Message => "Canceling an erronerous outcome object should have no effect");

         Test_Outcome.Mark_Done;
         Assert_Truth (Check   => (Test_Outcome.Status = Error) and (Test_Callback_Calls = 6),
                       Message => "Marking an erronerous outcome object as done should have no effect");
      end Test_Error_State;

   begin
      Setup_Tests;
      Test_Pending_State;
      Test_Done_State;
      Test_Canceled_State;
      Test_Error_State;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Outcomes.Implementation;
