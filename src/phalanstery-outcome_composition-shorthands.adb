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

with Ada.Exceptions;
with Phalanstery.Outcome_Composition.And_Gates;
with Phalanstery.Outcome_Composition.Interfaces;
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Outcomes.Servers,
                      Phalanstery.Utilities.Testing);

package body Phalanstery.Outcome_Composition.Shorthands is

   Done_Outcome : Outcomes.Clients.Client;

   function When_All (Wait_List : Interfaces.Valid_Outcome_List) return Interfaces.Valid_Outcome_Client is
   begin
      -- This implementation of When_All uses AND gates if needed, but takes a performance shortcut when possible.
      if Wait_List'Length > 1 then
         declare
            Gate : And_Gates.And_Gate;
            Mutable_Wait_List_Copy : Interfaces.Valid_Outcome_List := Wait_List;
         begin
            And_Gates.Add_Children (Gate, Mutable_Wait_List_Copy);
            return And_Gates.Make_Client (Gate);
         end;
      elsif Wait_List'Length = 1 then
         return Wait_List (Wait_List'First);
      else
         return Done_Outcome;
      end if;
   end When_All;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use all type Outcomes.Interfaces.Outcome_Status;
      subtype Valid_Outcome_Client is Interfaces.Valid_Outcome_Client;
      subtype Valid_Outcome_Server is Interfaces.Valid_Outcome_Server;
      subtype Valid_Outcome_List is Interfaces.Valid_Outcome_List;

      procedure Test_When_None is
         Empty_List : Valid_Outcome_List (2 .. 1);
         E : constant Valid_Outcome_Client := When_All (Empty_List);
      begin
         Assert_Truth (Check   => (E.Status = Done),
                       Message => "When_All should report a done outcome when waiting for nothing");
      end Test_When_None;

      procedure Test_When_One is
         Server : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         E : constant Valid_Outcome_Client := When_All ((1 => Server.Make_Client));
      begin
         Assert_Truth (Check   => (E.Status = Pending),
                       Message => "When_All should report a pending outcome when waiting for one pending operation");

         Server.Mark_Done;
         Assert_Truth (Check   => (E.Status = Done),
                       Message => "The outcome reported by When_All should follow that of the original operation");
      end Test_When_One;

      procedure Test_When_Done is
         Server1, Server2 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         E : constant Valid_Outcome_Client := When_All ((Server1.Make_Client, Server2.Make_Client));
      begin
         Server1.Mark_Done;
         Assert_Truth (Check   => (E.Status = Pending),
                       Message => "The output of When_All should still be Pending when only some operations are Done");

         Server2.Mark_Done;
         Assert_Truth (Check   => (E.Status = Done),
                       Message => "The output of When_All should switch to Done when all child operations are Done");
      end Test_When_Done;

      procedure Test_When_Canceled is
         Server1 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Server2 : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         E : constant Valid_Outcome_Client := When_All ((Server1.Make_Client, Server2.Make_Client));
      begin
         Server1.Cancel;
         Assert_Truth (Check   => (E.Status = Canceled),
                       Message => "The output of When_All should be Canceled when one child operation is Canceled");
      end Test_When_Canceled;

      procedure Test_When_Error is

         Server1 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Server2 : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         E : constant Valid_Outcome_Client := When_All ((Server1.Make_Client, Server2.Make_Client));

         Custom_Error : exception;
         Custom_Error_Occurrence : Ada.Exceptions.Exception_Occurrence;
         Test_Error : Ada.Exceptions.Exception_Occurrence;

      begin

         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurrence);
         Server1.Mark_Error (Custom_Error_Occurrence);
         Assert_Truth (Check   => (E.Status = Error),
                       Message => "The output of When_All should be erronerous when one child operation is erronerous");

         E.Get_Error (Test_Error);
         Assert_Truth (Check   => Utilities.Exceptions.Is_Occurrence_Of (Who  => Test_Error,
                                                                         What => Interfaces.Child_Error'Identity),
                       Message => "Child_Error should be propagated by When_All when a child operation is erronerous");

      end Test_When_Error;

   begin
      Test_When_None;
      Test_When_One;
      Test_When_Done;
      Test_When_Canceled;
      Test_When_Error;
   end Run_Tests;

begin

   -- Generate a done outcome object, to be used in When_All
   declare
      E : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
   begin
      E.Mark_Done;
      Done_Outcome := E.Make_Client;
   end;

   -- Conditionally run the unit tests on startup
   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Outcome_Composition.Shorthands;
