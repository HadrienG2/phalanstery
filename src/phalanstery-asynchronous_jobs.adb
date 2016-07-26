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
with Phalanstery.Outcome_Composition.Shorthands;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Asynchronous_Jobs is

   use all type Outcomes.Interfaces.Outcome_Status;

   function Return_Waiting (Cause : Valid_Outcome_Client) return Return_Value is
     ((State => Waiting, Awaited_Outcome => Cause));

   function Return_Waiting (Cause : Valid_Outcome_List) return Return_Value is
     ((State => Waiting, Awaited_Outcome => Outcome_Composition.Shorthands.When_All (Cause)));

   function Status (What : Return_Value) return Return_Status is
     (What.State);

   function Awaited_Outcome (What : Return_Value) return Valid_Outcome_Client is
     (What.Awaited_Outcome);

   function Handle_Aborted_Dependency (Who               : in out Asynchronous_Job;
                                       Dependency_Status : Aborted_Outcome_Status) return Return_Value is
     ( case Dependency_Status is
         when Canceled =>
            Return_Canceled,
         when Error =>
            raise Dependency_Error );


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
               Unused : constant Valid_Outcome_Client := Awaited_Outcome (Return_Finished) with Unreferenced;
            begin
               Fail ("A finished job should not wait for something");
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
               Unused : constant Valid_Outcome_Client := Awaited_Outcome (Return_Yielding) with Unreferenced;
            begin
               Fail ("A yielding job should not wait for something");
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
            Awaited : constant Valid_Outcome_Client := Awaited_Outcome (R);
         begin
            Assert_Truth (Check   => (Awaited = C),
                          Message => "A job waiting for one operation should wait for the right one");
         end;
      end Test_Waiting_One;

      procedure Test_Waiting_Multiple is
         E1, E2 : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         C1 : constant Valid_Outcome_Client := E1.Make_Client;
         C2 : constant Valid_Outcome_Client := E2.Make_Client;
         R : constant Return_Value := Return_Waiting ((C1, C2));
      begin
         Assert_Truth (Check   => (Status (R) = Waiting),
                       Message => "The status of a waiting job should be Waiting");
         declare
            Awaited : constant Valid_Outcome_Client := Awaited_Outcome (R);
         begin
            Assert_Truth (Check   => (Awaited.Status = Pending),
                          Message => "Initially, a job waiting for two operations should be pending");

            E1.Mark_Done;
            Assert_Truth (Check   => (Awaited.Status = Pending),
                          Message => "Fulfilling only part of the dependencies of a waiting job should not be enough");

            E2.Mark_Done;
            Assert_Truth (Check   => (Awaited.Status = Done),
                          Message => "Once all the dependencies of a job are fulfilled, the job should be ready");
         end;
      end Test_Waiting_Multiple;

      procedure Test_Canceled is
      begin
         Assert_Truth (Check   => (Status (Return_Canceled) = Canceled),
                       Message => "The status of a job returning Return_Canceled should be Canceled");
         begin
            declare
               Unused : constant Valid_Outcome_Client := Awaited_Outcome (Return_Canceled) with Unreferenced;
            begin
               Fail ("A canceled job should not wait for something");
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
