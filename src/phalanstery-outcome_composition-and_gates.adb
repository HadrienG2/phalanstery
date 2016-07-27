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
with Ada.Exceptions;
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Exceptions,
                      Phalanstery.Utilities.Testing);

package body Phalanstery.Outcome_Composition.And_Gates is

   use all type Outcomes.Interfaces.Final_Outcome_Status;

   Child_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

   protected body And_Gate_Implementation is

      procedure Notify_Child_Outcome (What : Outcomes.Interfaces.Final_Outcome_Status) is
      begin
         case What is
            when Done =>
               Done_Children := Done_Children + 1;
            when Canceled =>
               Current_Status := Canceled;
            when Error =>
               Current_Status := Error;
         end case;
         Propagate_Outcome;
      end Notify_Child_Outcome;

      procedure Add_Children (Count : Natural) is
      begin
         if not Frozen then
            Child_Count := Child_Count + Count;
            -- NOTE : Cannot add ourselves as listener here, this will be a job for And_Gate_Child_Listener
         else
            raise Interfaces.Composite_Outcome_Already_Frozen;
         end if;
      end Add_Children;

      procedure Make_Client (Where : out Interfaces.Valid_Outcome_Client) is
      begin
         Frozen := True;
         Where := Outcome.Make_Client;
         Propagate_Outcome;
      end Make_Client;

      function Is_Frozen return Boolean is (Frozen);

      procedure Propagate_Outcome is
      begin
         -- Do not decide on a final outcome until all children have been added
         if not Frozen then
            return;
         end if;

         -- Propagate the final outcome
         case Current_Status is
            when Pending =>
               if Done_Children = Child_Count then
                  Current_Status := Done;
                  Outcome.Mark_Done;
               end if;
            when Done =>
               raise Ada.Assertions.Assertion_Error with "This line of code should never be reached";
            when Canceled =>
               Outcome.Cancel;
            when Error =>
               Outcome.Mark_Error (Child_Error_Occurence);
         end case;
      end Propagate_Outcome;

   end And_Gate_Implementation;

   not overriding procedure Add_Child (Where : in out And_Gate;
                                       Who   : Interfaces.Valid_Outcome_Client) is
      Child_Listener : And_Gate_Child_Listener := (Ref => Where.Ref);
   begin
      Where.Ref.Set.Add_Children (1);
      Who.Add_Listener (Child_Listener);
   end Add_Child;

   not overriding procedure Add_Children (Where : in out And_Gate;
                                          Who   : Interfaces.Valid_Outcome_List) is
      Child_Listener : And_Gate_Child_Listener := (Ref => Where.Ref);
   begin
      Where.Ref.Set.Add_Children (Who'Length);
      for Outcome of Who loop
         Outcome.Add_Listener (Child_Listener);
      end loop;
   end Add_Children;

   not overriding function Make_Client (From : in out And_Gate) return Interfaces.Valid_Outcome_Client is
      C : Outcomes.Clients.Client;
   begin
      From.Ref.Set.Make_Client (C);
      return C;
   end Make_Client;

   overriding function Is_Frozen (What : And_Gate) return Boolean is (What.Ref.Get.Is_Frozen);

   overriding procedure Notify_Outcome (Where : in out And_Gate_Child_Listener;
                                        What  : Outcomes.Interfaces.Final_Outcome_Status) is
   begin
      Where.Ref.Set.Notify_Child_Outcome (What);
   end Notify_Outcome;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Exceptions.Exception_Id;
      subtype Valid_Outcome_Client is Interfaces.Valid_Outcome_Client;
      subtype Valid_Outcome_Server is Interfaces.Valid_Outcome_Server;
      subtype Valid_Outcome_List is Interfaces.Valid_Outcome_List;

      Test_Error : Ada.Exceptions.Exception_Occurrence;
      Custom_Error : exception;
      Custom_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

      procedure Setup_Tests is
      begin
         Utilities.Exceptions.Make_Occurrence (Custom_Error'Identity,
                                               Custom_Error_Occurence);
      end Setup_Tests;

      procedure Test_Initial_State is
         Test_Gate : And_Gate;
         Test_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
      begin
         Assert_Truth (Check   => (Test_Client.Status = Done),
                       Message => "An AND gate with no children should be Done");
      end Test_Initial_State;

      procedure Test_Done_Child is
         Test_Gate : And_Gate;
         Test_Child_Server : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Client : constant Valid_Outcome_Client := Test_Child_Server.Make_Client;
      begin
         Test_Gate.Add_Child (Test_Child_Client);

         declare
            Test_Gate_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
         begin
            Assert_Truth (Check   => (Test_Gate_Client.Status = Pending),
                          Message => "An AND gate with one pending child should be Pending");

            begin
               Test_Gate.Add_Child (Test_Child_Client);
               Fail ("Adding children to a frozen AND gate should be forbidden");
            exception
               when Ada.Assertions.Assertion_Error | Interfaces.Composite_Outcome_Already_Frozen =>
                  null;
            end;

            Test_Child_Server.Mark_Done;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Done),
                          Message => "An AND gate whose children are Done should be Done");
         end;
      end Test_Done_Child;

      procedure Test_Canceled_Child is
         Test_Gate : And_Gate;
         Test_Child_Server : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Client : constant Valid_Outcome_Client := Test_Child_Server.Make_Client;
      begin
         Test_Gate.Add_Child (Test_Child_Client);
         declare
            Test_Gate_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server.Cancel;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Canceled),
                          Message => "An AND gate with a canceled child should be Canceled");
         end;
      end Test_Canceled_Child;

      procedure Test_Child_Error is
         Test_Gate : And_Gate;
         Test_Child_Server : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Client : constant Valid_Outcome_Client := Test_Child_Server.Make_Client;
      begin
         Test_Gate.Add_Child (Test_Child_Client);

         declare
            Test_Gate_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server.Mark_Error (Custom_Error_Occurence);
            Assert_Truth (Check   => (Test_Gate_Client.Status = Error),
                          Message => "An AND gate with an erronerous child should be in the Error state");

            Test_Gate_Client.Get_Error (Test_Error);
            Assert_Truth (Check   => Utilities.Exceptions.Is_Occurrence_Of (Who  => Test_Error,
                                                                            What => Interfaces.Child_Error'Identity),
                          Message => "The error associated with an erronerous AND gate should be Child_Error");
         end;
      end Test_Child_Error;

      procedure Test_Done_Children is
         Test_Gate : And_Gate;
         Test_Child_Server_1, Test_Child_Server_2 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Clients : constant Valid_Outcome_List (1 .. 2) := (Test_Child_Server_1.Make_Client,
                                                                       Test_Child_Server_2.Make_Client);
      begin
         Test_Child_Server_1.Mark_Done;
         Test_Gate.Add_Children (Test_Child_Clients);

         declare
            Test_Gate_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
         begin
            Assert_Truth (Check   => (Test_Gate_Client.Status = Pending),
                          Message => "An AND gate with only some Done children should still be Pending");

            Test_Child_Server_2.Mark_Done;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Done),
                          Message => "An AND gate with only Done children should be Done");
         end;
      end Test_Done_Children;

      procedure Test_Canceled_Children is
         Test_Gate : And_Gate;
         Test_Child_Server_1 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Server_2 : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Clients : constant Valid_Outcome_List (1 .. 2) := (Test_Child_Server_1.Make_Client,
                                                                       Test_Child_Server_2.Make_Client);
      begin
         Test_Gate.Add_Children (Test_Child_Clients);
         declare
            Test_Gate_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server_1.Cancel;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Canceled),
                          Message => "An AND gate with one canceled child should be Canceled");
         end;
      end Test_Canceled_Children;

      procedure Test_Children_Error is
         Test_Gate : And_Gate;
         Test_Child_Server_1 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Server_2 : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Test_Child_Clients : constant Valid_Outcome_List (1 .. 2) := (Test_Child_Server_1.Make_Client,
                                                                       Test_Child_Server_2.Make_Client);
      begin
         Test_Gate.Add_Children (Test_Child_Clients);

         declare
            Test_Gate_Client : constant Valid_Outcome_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server_1.Mark_Error (Custom_Error_Occurence);
            Assert_Truth (Check   => (Test_Gate_Client.Status = Error),
                          Message => "An AND gate with one erronerous child should be in the Error state");

            Test_Gate_Client.Get_Error (Test_Error);
            Assert_Truth (Check   => Utilities.Exceptions.Is_Occurrence_Of (Who  => Test_Error,
                                                                            What => Interfaces.Child_Error'Identity),
                          Message => "The error associated with an erronerous AND gate should be Child_Error");
         end;
      end Test_Children_Error;

   begin
      Setup_Tests;
      Test_Initial_State;
      Test_Done_Child;
      Test_Canceled_Child;
      Test_Child_Error;
      Test_Done_Children;
      Test_Canceled_Children;
      Test_Children_Error;
   end Run_Tests;

begin

   -- Save an occurence of Child_Error, to be propagated as needed
   Utilities.Exceptions.Make_Occurrence (What  => Interfaces.Child_Error'Identity,
                                         Where => Child_Error_Occurence);

   -- Conditionally run the unit tests on startup
   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Outcome_Composition.And_Gates;
