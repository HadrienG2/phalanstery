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
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Exceptions,
                      Phalanstery.Utilities.Testing);

package body Phalanstery.Events.Composition.And_Gates is

   Child_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

   protected body And_Gate_Implementation is

      procedure Notify_Event_Status_Change (What : Interfaces.Finished_Event_Status) is
      begin
         case What is
            when Done =>
               Done_Children := Done_Children + 1;
            when Canceled =>
               Current_Status := Canceled;
            when Error =>
               Current_Status := Error;
         end case;
         Propagate_Status_Change;
      end Notify_Event_Status_Change;

      procedure Add_Children (Count : Natural) is
      begin
         if not Frozen then
            Child_Count := Child_Count + Count;
            -- NOTE : Cannot add ourselves as listener here, this will be a job for the reference
         else
            raise Composite_Event_Already_Frozen;
         end if;
      end Add_Children;

      procedure Make_Client (Where : out Valid_Event_Client) is
      begin
         Frozen := True;
         Where := Event.Make_Client;
         Propagate_Status_Change;
      end Make_Client;

      function Is_Frozen return Boolean is (Frozen);

      procedure Propagate_Status_Change is
      begin
         -- Do not propagate event status if clients may still be added
         if not Frozen then
            return;
         end if;

         -- Propagate the current event status
         case Current_Status is
            when Pending =>
               if Done_Children = Child_Count then
                  Current_Status := Done;
                  Event.Mark_Done;
               end if;
            when Done =>
               raise Ada.Assertions.Assertion_Error;  -- This case should never be reached
            when Canceled =>
               Event.Cancel;
            when Error =>
               Event.Mark_Error (Child_Error_Occurence);
         end case;
      end Propagate_Status_Change;

   end And_Gate_Implementation;

   procedure Add_Child (Where : in out And_Gate;
                        Who   : in out Valid_Event_Client) is
   begin
      Where.Ref.Set.Add_Children (1);
      Who.Add_Listener (Where);
   end Add_Child;

   procedure Add_Children (Where : in out And_Gate;
                           Who   : in out Valid_Event_List) is
   begin
      Where.Ref.Set.Add_Children (Who'Length);
      for Event of Who loop
         Event.Add_Listener (Where);
      end loop;
   end Add_Children;

   function Make_Client (From : in out And_Gate) return Valid_Event_Client is
      C : Events.Clients.Client;
   begin
      From.Ref.Set.Make_Client (C);
      return C;
   end Make_Client;

   function Is_Frozen (What : And_Gate) return Boolean is (What.Ref.Get.Is_Frozen);

   overriding procedure Notify_Event_Status_Change (Where : in out And_Gate;
                                                    What  : Interfaces.Finished_Event_Status) is
   begin
      Where.Ref.Set.Notify_Event_Status_Change (What);
   end Notify_Event_Status_Change;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Exceptions.Exception_Id;

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
         Test_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
      begin
         Assert_Truth (Check   => (Test_Client.Status = Done),
                       Message => "An AND gate with no children should be Done");
      end Test_Initial_State;

      procedure Test_Done_Child is
         Test_Gate : And_Gate;
         Test_Child_Server : Valid_Event_Server := Servers.Make_Event;
         Test_Child_Client : Valid_Event_Client := Test_Child_Server.Make_Client;
      begin
         Test_Gate.Add_Child (Test_Child_Client);

         declare
            Test_Gate_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
         begin
            Assert_Truth (Check   => (Test_Gate_Client.Status = Pending),
                          Message => "An AND gate with one pending child should be Pending");

            begin
               Test_Gate.Add_Child (Test_Child_Client);
               Fail ("Adding clients to a frozen AND gate should be forbidden");
            exception
               when Ada.Assertions.Assertion_Error | Composite_Event_Already_Frozen =>
                  null;
            end;

            Test_Child_Server.Mark_Done;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Done),
                          Message => "An AND gate whose children are Done should be Done");
         end;
      end Test_Done_Child;

      procedure Test_Canceled_Child is
         Test_Gate : And_Gate;
         Test_Child_Server : Valid_Event_Server := Servers.Make_Event;
         Test_Child_Client : Valid_Event_Client := Test_Child_Server.Make_Client;
      begin
         Test_Gate.Add_Child (Test_Child_Client);
         declare
            Test_Gate_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server.Cancel;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Canceled),
                          Message => "An AND gate with a canceled child should be Canceled");
         end;
      end Test_Canceled_Child;

      procedure Test_Child_Error is
         Test_Gate : And_Gate;
         Test_Child_Server : Valid_Event_Server := Servers.Make_Event;
         Test_Child_Client : Valid_Event_Client := Test_Child_Server.Make_Client;
      begin
         Test_Gate.Add_Child (Test_Child_Client);

         declare
            Test_Gate_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server.Mark_Error (Custom_Error_Occurence);
            Assert_Truth (Check   => (Test_Gate_Client.Status = Error),
                          Message => "An AND gate with an erronerous child should be in the Error state");

            Test_Gate_Client.Get_Error (Test_Error);
            Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Child_Error'Identity),
                          Message => "The error associated with an erronerous AND gate should be Child_Error");
         end;
      end Test_Child_Error;

      procedure Test_Done_Children is
         Test_Gate : And_Gate;
         Test_Child_Server_1, Test_Child_Server_2 : Valid_Event_Server := Servers.Make_Event;
         Test_Child_Clients : Valid_Event_List (1 .. 2) := (Test_Child_Server_1.Make_Client,
                                                            Test_Child_Server_2.Make_Client);
      begin
         Test_Child_Server_1.Mark_Done;
         Test_Gate.Add_Children (Test_Child_Clients);

         declare
            Test_Gate_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
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
         Test_Child_Server_1 : Valid_Event_Server := Servers.Make_Event;
         Test_Child_Server_2 : constant Valid_Event_Server := Servers.Make_Event;
         Test_Child_Clients : Valid_Event_List (1 .. 2) := (Test_Child_Server_1.Make_Client,
                                                            Test_Child_Server_2.Make_Client);
      begin
         Test_Gate.Add_Children (Test_Child_Clients);
         declare
            Test_Gate_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server_1.Cancel;
            Assert_Truth (Check   => (Test_Gate_Client.Status = Canceled),
                          Message => "An AND gate with one canceled child should be Canceled");
         end;
      end Test_Canceled_Children;

      procedure Test_Children_Error is
         Test_Gate : And_Gate;
         Test_Child_Server_1 : Valid_Event_Server := Servers.Make_Event;
         Test_Child_Server_2 : constant Valid_Event_Server := Servers.Make_Event;
         Test_Child_Clients : Valid_Event_List (1 .. 2) := (Test_Child_Server_1.Make_Client,
                                                            Test_Child_Server_2.Make_Client);
      begin
         Test_Gate.Add_Children (Test_Child_Clients);

         declare
            Test_Gate_Client : constant Valid_Event_Client := Test_Gate.Make_Client;
         begin
            Test_Child_Server_1.Mark_Error (Custom_Error_Occurence);
            Assert_Truth (Check   => (Test_Gate_Client.Status = Error),
                          Message => "An AND gate with one erronerous child should be in the Error state");

            Test_Gate_Client.Get_Error (Test_Error);
            Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Child_Error'Identity),
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
   Utilities.Exceptions.Make_Occurrence (What  => Child_Error'Identity,
                                         Where => Child_Error_Occurence);

   -- Conditionally run the unit tests on startup
   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Events.Composition.And_Gates;