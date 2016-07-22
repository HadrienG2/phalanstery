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
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Asynchronous_Jobs is

   function Return_Waiting (Cause : Valid_Outcome_Client) return Return_Value is
     ((State => Waiting, Wait_List_Length => 1, Wait_List => (1 => Cause)));

   function Return_Waiting (Cause : Valid_Outcome_List) return Return_Value is
     ((State => Waiting, Wait_List_Length => Cause'Length, Wait_List => Cause));

   function Status (What : Return_Value) return Return_Status is
     (What.State);

   function Wait_List (What : Return_Value) return Valid_Outcome_List is
     (What.Wait_List);


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use type Valid_Outcome_Client;
      subtype Valid_Outcome_Server is Outcomes.Contracts.Valid_Outcome_Server;

      procedure Test_Finished is
      begin
         Assert_Truth (Check   => (Status (Return_Finished) = Finished),
                       Message => "The status of a job returning Return_Finished should be Finished");
         begin
            declare
               Unused : constant Valid_Outcome_List := Wait_List (Return_Finished) with Unreferenced;
            begin
               Fail ("Querying the wait list of a finished job should be an error");
            end;
         exception
            when Ada.Assertions.Assertion_Error | Constraint_Error =>
               null;
         end;
      end Test_Finished;

      procedure Test_Yielding is
      begin
         Assert_Truth (Check   => (Status (Return_Yielding) = Yielding),
                       Message => "The status of a job returning Return_Yielding should be Yielding");
         begin
            declare
               Unused : constant Valid_Outcome_List := Wait_List (Return_Yielding) with Unreferenced;
            begin
               Fail ("Querying the wait list of a yielding job should be an error");
            end;
         exception
            when Ada.Assertions.Assertion_Error | Constraint_Error =>
               null;
         end;
      end Test_Yielding;

      procedure Test_Waiting_One is
         E : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         C : constant Valid_Outcome_Client := E.Make_Client;
         R : constant Return_Value := Return_Waiting (C);
      begin
         Assert_Truth (Check   => (Status (R) = Waiting),
                       Message => "The status of a waiting job should be Waiting");
         declare
            List : constant Valid_Outcome_List := Wait_List (R);
         begin
            Assert_Truth (Check   => ((List'Length = 1) and then (List (List'First) = C)),
                          Message => "The wait list of a job waiting for one operation should be correct");
         end;
      end Test_Waiting_One;

      procedure Test_Waiting_Multiple is
         E1, E2 : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         C1 : constant Valid_Outcome_Client := E1.Make_Client;
         C2 : constant Valid_Outcome_Client := E2.Make_Client;
         R : constant Return_Value := Return_Waiting ((C1, C2));
      begin
         Assert_Truth (Check   => (Status (R) = Waiting),
                       Message => "The status of a waiting job should be Waiting");
         declare
            List : constant Valid_Outcome_List := Wait_List (R);
         begin
            Assert_Truth (Check   => ((List'Length = 2) and then
                                        ((List (List'First) = C1) and (List (List'Last) = C2))),
                          Message => "The wait list of a job waiting for several operations should be correct");
         end;
      end Test_Waiting_Multiple;

      procedure Test_Canceled is
      begin
         Assert_Truth (Check   => (Status (Return_Canceled) = Canceled),
                       Message => "The status of a job returning Return_Canceled should be Canceled");
         begin
            declare
               Unused : constant Valid_Outcome_List := Wait_List (Return_Canceled) with Unreferenced;
            begin
               Fail ("Querying the wait list of a canceled job should be an error");
            end;
         exception
            when Ada.Assertions.Assertion_Error | Constraint_Error =>
               null;
         end;
      end Test_Canceled;

   begin
      Test_Finished;
      Test_Yielding;
      Test_Waiting_One;
      Test_Waiting_Multiple;
      Test_Canceled;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Asynchronous_Jobs;
