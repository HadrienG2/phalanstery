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

with Ada.Unchecked_Deallocation;
with Phalanstery.Examples.Trivial_Jobs;
with Phalanstery.Outcome_Composition.Shorthands;
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.Testing;
with System.Multiprocessors;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.SMP.Executor_Objects is

   overriding procedure Schedule_Job (Where : in out Executor;
                                      What : Interfaces.Any_Asynchronous_Job) is
      Unused : constant Interfaces.Valid_Outcome_Client := Schedule_Job (Where => Where,
                                                                         What  => What) with Unreferenced;
   begin
      null;
   end Schedule_Job;

   overriding procedure Schedule_Job (Where : in out Executor;
                                      What  : Interfaces.Any_Asynchronous_Job;
                                      After : Interfaces.Valid_Outcome_Client) is
      Unused : constant Interfaces.Valid_Outcome_Client := Schedule_Job (Where => Where,
                                                                         What  => What,
                                                                         After => After) with Unreferenced;
   begin
      null;
   end Schedule_Job;

   overriding procedure Schedule_Job (Where : in out Executor;
                                      What  : Interfaces.Any_Asynchronous_Job;
                                      After : Interfaces.Valid_Outcome_List) is
      Unused : constant Interfaces.Valid_Outcome_Client := Schedule_Job (Where => Where,
                                                                         What  => What,
                                                                         After => After) with Unreferenced;
   begin
      null;
   end Schedule_Job;

   overriding function Schedule_Job (Where : in out Executor;
                                     What : Interfaces.Any_Asynchronous_Job) return Interfaces.Valid_Outcome_Client is
      Empty_Wait_List : Interfaces.Valid_Outcome_List (2 .. 1);
   begin
      return Schedule_Job (Where => Where,
                           What  => What,
                           After => Empty_Wait_List);
   end Schedule_Job;

   overriding function Schedule_Job (Where : in out Executor;
                                     What  : Interfaces.Any_Asynchronous_Job;
                                     After : Interfaces.Valid_Outcome_Client) return Interfaces.Valid_Outcome_Client is
      Result : Outcomes.Clients.Client;
   begin
      Where.Executor_Task.Schedule_Job (What    => What,
                                        After   => After,
                                        Outcome => Result);
      return Result;
   end Schedule_Job;

   overriding function Schedule_Job (Where : in out Executor;
                                     What  : Interfaces.Any_Asynchronous_Job;
                                     After : Interfaces.Valid_Outcome_List) return Interfaces.Valid_Outcome_Client is
   begin
      return Schedule_Job (Where => Where,
                           What  => What,
                           After => Outcome_Composition.Shorthands.When_All (After));
   end Schedule_Job;

   overriding procedure Initialize (Who : in out Executor) is
      subtype Executor_Task_Type is Executor_Task (Who.Number_Of_Workers);
   begin
      Who.Executor_Task := new Executor_Task_Type;
   end Initialize;

   overriding procedure Finalize (Who : in out Executor) is
      procedure Free_Executor is new Ada.Unchecked_Deallocation (Executor_Task, Executor_Access);
   begin
      if Who.Executor_Task /= null then
         Who.Executor_Task.Stop;
         loop
            exit when Who.Executor_Task'Terminated;
            delay 0.02;
         end loop;
         Free_Executor (Who.Executor_Task);
      end if;
   end Finalize;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use type System.Multiprocessors.CPU_Range;
      use all type Outcomes.Interfaces.Final_Outcome_Status;

      Number_Of_Workers : constant := 2;
      Test_Executor : Executor (Number_Of_Workers);
      T : Examples.Trivial_Jobs.Null_Job;

      procedure Test_Initial_State is
      begin
         Assert_Truth (Check   => (Test_Executor.Executor_Task /= null),
                       Message => "An executor task should be spawned on executor object creation");
         Assert_Truth (Check   => (Test_Executor.Executor_Task.Number_Of_Workers = Number_Of_Workers),
                       Message => "The executor task of an executor should have the right number of workers");
      end Test_Initial_State;

      procedure Test_Functions is
         Dep1_S, Dep2_S : constant Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Dep1_C : constant Interfaces.Valid_Outcome_Client := Dep1_S.Make_Client;
         Dep2_C : constant Interfaces.Valid_Outcome_Client := Dep2_S.Make_Client;
      begin

         declare
            C : constant Interfaces.Valid_Outcome_Client := Test_Executor.Schedule_Job (What => T);
         begin
            C.Wait_Completion;
            Assert_Truth (Check   => (C.Status = Done),
                          Message => "The null job should complete properly after being scheduled");
         end;

         declare
            C : constant Interfaces.Valid_Outcome_Client := Test_Executor.Schedule_Job (What  => T,
                                                                                        After => Dep1_C);
         begin
            Assert_Truth (Check   => (C.Status = Pending),
                          Message => "The null job should not start until its dependencies are satisfied");

            Dep1_S.Mark_Done;
            C.Wait_Completion;
            Assert_Truth (Check   => (C.Status = Done),
                          Message => "The null job should complete properly after its dependencies are met");
         end;

         declare
            C : constant Interfaces.Valid_Outcome_Client := Test_Executor.Schedule_Job (What  => T,
                                                                                        After => (Dep1_C, Dep2_C));
         begin
            Assert_Truth (Check   => (C.Status = Pending),
                          Message => "The null job should not start until all its dependencies are satisfied");

            Dep2_S.Mark_Done;
            C.Wait_Completion;
            Assert_Truth (Check   => (C.Status = Done),
                          Message => "The null job should complete properly after its dependencies are met");
         end;

      end Test_Functions;

      procedure Test_Procedures is
         Alternate_Executor : Executor (Number_Of_Workers);
         S1, S2 : constant Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         C1 : constant Interfaces.Valid_Outcome_Client := S1.Make_Client;
         C2 : constant Interfaces.Valid_Outcome_Client := S2.Make_Client;
      begin
         -- Fire-and forget execution is particularly challenging to test, as we have no idea when it will occur and
         -- have no way to synchronize with it. Consequently, we only test that it does not hang or crash.
         Alternate_Executor.Schedule_Job (What => T);
         Alternate_Executor.Schedule_Job (What  => T,
                                          After => C1);
         Alternate_Executor.Schedule_Job (What  => T,
                                          After => (C1, C2));
         S1.Mark_Done;
         S2.Mark_Done;
      end Test_Procedures;

      procedure Test_Finalization is
      begin
         -- Executors should be resilient to multiple finalization
         Test_Executor.Finalize;
         Test_Executor.Finalize;
      end Test_Finalization;

   begin
      Test_Initial_State;
      Test_Functions;
      Test_Procedures;
      Test_Finalization;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.SMP.Executor_Objects;
