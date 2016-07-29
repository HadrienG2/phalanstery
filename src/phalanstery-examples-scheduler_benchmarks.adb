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

with Ada.Calendar;
with Phalanstery.Asynchronous_Jobs;
with Phalanstery.Examples.Trivial_Jobs;
with Phalanstery.Executors.Interfaces;
with Phalanstery.Executors.SMP.Executor_Objects;
with Phalanstery.Outcome_Composition.Interfaces;
with Phalanstery.Outcome_Composition.Shorthands;
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Contracts;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Outcomes.Servers;
with Ada.Text_IO;
with System.Multiprocessors;
pragma Elaborate_All (Phalanstery.Outcomes.Servers);

package body Phalanstery.Examples.Scheduler_Benchmarks is

   procedure Run_Benchmarks is

      use type Ada.Calendar.Time;

      subtype Executor is Executors.SMP.Executor_Objects.Executor;
      subtype Outcome_Client is Outcomes.Clients.Client;
      subtype Valid_Outcome_Client is Outcomes.Contracts.Valid_Outcome_Client;
      subtype Valid_Outcome_Server is Outcomes.Contracts.Valid_Outcome_Server;

      Parallel_Executor : Executor;
      Serial_Executor : Executor (1);
      Number_Of_CPUs : constant Positive := Positive (System.Multiprocessors.Number_Of_CPUs);

      procedure Benchmark_Job (What                : Executors.Interfaces.Any_Asynchronous_Job;
                               How_Many            : Positive;
                               Title               : String;
                               Feature_Name        : String;
                               Internal_Iterations : Positive := 1) is

         Test_Outcome : Outcome_Client;

         Start_Time, Outcome_Init_Time, Schedule_Time, When_All_Time, Wait_Over_Time, End_Time : Ada.Calendar.Time;
         Parallel_Duration, Sequential_Duration, Direct_Run_Duration : Duration;

         procedure Run_Serial_Tests (On : in out Executor;
                                     Executor_Kind : String) is
         begin
            -- Run the serial tests
            Start_Time := Ada.Calendar.Clock;
            for I in 1 .. How_Many loop
               Test_Outcome := On.Schedule_Job (What, Test_Outcome);
            end loop;
            Schedule_Time := Ada.Calendar.Clock;
            Test_Outcome.Wait_Completion;
            End_Time := Ada.Calendar.Clock;

            -- Analyze sequential results
            Sequential_Duration := End_Time - Start_Time;
            Ada.Text_IO.Put_Line ("Serial run on " & Executor_Kind & " executor took " &
                                    Duration'Image (Sequential_Duration) & " s (" &
                                    Duration'Image (Sequential_Duration / Parallel_Duration) & "x parallel run):");
            Ada.Text_IO.Put_Line ("   - Scheduling took " & Duration'Image (Schedule_Time - Start_Time) & " s.");
            Ada.Text_IO.Put_Line ("   - Synchronization took " & Duration'Image (End_Time - Schedule_Time) & " s.");
         end Run_Serial_Tests;

      begin

         -- Parallel version
         Ada.Text_IO.Put_Line ("=== Testing the executor's " & Title & " performance ===");
         Start_Time := Ada.Calendar.Clock;
         declare
            Parallel_Outcomes : Outcome_Composition.Interfaces.Outcome_List (1 .. How_Many);
         begin
            Outcome_Init_Time := Ada.Calendar.Clock;
            Parallel_Outcomes := (others => Parallel_Executor.Schedule_Job (What));
            Schedule_Time := Ada.Calendar.Clock;
            Test_Outcome := Outcome_Composition.Shorthands.When_All (Parallel_Outcomes);
            When_All_Time := Ada.Calendar.Clock;
            Test_Outcome.Wait_Completion;
            Wait_Over_Time := Ada.Calendar.Clock;
         end;
         End_Time := Ada.Calendar.Clock;

         -- Analyze parallel results
         Parallel_Duration := End_Time - Start_Time;
         Ada.Text_IO.Put_Line ("Parallel run took " & Duration'Image (Parallel_Duration) & " s:");
         Ada.Text_IO.Put_Line ("   - Outcome creation took " & Duration'Image (Outcome_Init_Time - Start_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Scheduling took " & Duration'Image (Schedule_Time - Outcome_Init_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Composition took " & Duration'Image (When_All_Time - Schedule_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Synchronization took " & Duration'Image (Wait_Over_Time - When_All_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Outcome liberation took " & Duration'Image (End_Time - Wait_Over_Time) & " s.");

         -- Serial version
         Run_Serial_Tests (Parallel_Executor, "parallel");
         Run_Serial_Tests (Serial_Executor, "serial");

         -- Direct run (no executor)
         Start_Time := Ada.Calendar.Clock;
         Job_Loop :
         for I in 1 .. How_Many loop
            declare
               Job_Copy : Executors.Interfaces.Any_Asynchronous_Job := What;
            begin
               Direct_Execution_Loop :
               loop
                  declare
                     use all type Asynchronous_Jobs.Return_Status;
                     Result : constant Asynchronous_Jobs.Return_Value := Job_Copy.Run (Was_Canceled => False);
                  begin
                     case Asynchronous_Jobs.Status (Result) is
                        when Finished =>
                           exit Direct_Execution_Loop;
                        when Yielding =>
                           null;
                        when Canceled =>
                           exit Job_Loop;
                        when Waiting =>
                           declare
                              use all type Outcomes.Interfaces.Outcome_Status;
                              E : constant Valid_Outcome_Client := Asynchronous_Jobs.Awaited_Outcome (Result);
                           begin
                              case E.Status is
                                 when Pending =>
                                    raise Constraint_Error with "Direct runs cannot handle truly blocking jobs";
                                 when Done =>
                                    null;
                                 when Canceled | Error =>
                                    exit Job_Loop;
                              end case;
                           end;
                     end case;
                  end;
               end loop Direct_Execution_Loop;
            end;
         end loop Job_Loop;
         Direct_Run_Duration := Ada.Calendar.Clock - Start_Time;
         Ada.Text_IO.Put_Line ("Direct run took " & Duration'Image (Direct_Run_Duration) & " s");

         -- Estimate scheduling overhead
         declare
            Total_Scheduling_Duration : constant Duration := Sequential_Duration - Direct_Run_Duration;
            Scheduling_Overhead : constant Float :=
              Float (Total_Scheduling_Duration) / (Float (How_Many * Internal_Iterations));
            type Microsecs is delta 0.001 range -1000.0 .. 1000.0;
            Rounded_Overhead : constant Microsecs := Microsecs (Scheduling_Overhead * 10.0**6);
         begin
            Ada.Text_IO.Put_Line ("The overhead of " & Feature_Name & " is about " &
                                    Microsecs'Image (Rounded_Overhead) & " microseconds");
         end;
         Ada.Text_IO.New_Line;

      end Benchmark_Job;

      procedure Benchmark_Null_Job is
         My_Job : Examples.Trivial_Jobs.Null_Job;
      begin
         Benchmark_Job (What         => My_Job,
                        How_Many     => 150_000,
                        Title        => "null job",
                        Feature_Name => "running the null job");
      end Benchmark_Null_Job;

      procedure Benchmark_Yielding is
         Yielding_Iterations : constant := 50_000;
         My_Job : Examples.Trivial_Jobs.Yielding_Job (Yielding_Iterations);
      begin

         Benchmark_Job (What                => My_Job,
                        How_Many            => 2 * Number_Of_CPUs,
                        Title               => "yielding",
                        Feature_Name        => "yielding in a job",
                        Internal_Iterations => Yielding_Iterations);
      end Benchmark_Yielding;

      Busy_Waiting_Microseconds : constant := 100;

      procedure Benchmark_Waiting is
         My_Job : Examples.Trivial_Jobs.Waiting_Job (Busy_Waiting_Microseconds);
      begin

         Benchmark_Job (What                => My_Job,
                        How_Many            => 50_000,
                        Title               => "busy-waiting",
                        Feature_Name        => "running a busy-waiting job");
      end Benchmark_Waiting;

      procedure Benchmark_Wait_Ready is
         My_Job : Examples.Trivial_Jobs.Ready_Wait_Job (Busy_Waiting_Microseconds);
      begin
         Benchmark_Job (What         => My_Job,
                        How_Many     => 50_000,
                        Title        => "ready wait",
                        Feature_Name => "waiting for a ready outcome");
      end Benchmark_Wait_Ready;

      procedure Benchmark_Wait_Custom is

         Outcome_Server_S : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Outcome_Server_P : constant Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Outcome_Client_P : constant Valid_Outcome_Client := Outcome_Server_P.Make_Client;
         Outcome_Client_S : constant Valid_Outcome_Client := Outcome_Server_S.Make_Client;

         subtype Custom_Wait_Job is Examples.Trivial_Jobs.Custom_Wait_Job;
         subtype Cancelation_Job is Examples.Trivial_Jobs.Cancelation_Job;
         Consumer_Job_P : constant Custom_Wait_Job := (Waiting_Microseconds => Busy_Waiting_Microseconds,
                                                       Target => Outcome_Client_P);
         Consumer_Job_S : constant Custom_Wait_Job := (Waiting_Microseconds => Busy_Waiting_Microseconds,
                                                       Target => Outcome_Client_S);
         Producer_Job_P : constant Cancelation_Job := (Waiting_Microseconds => Busy_Waiting_Microseconds,
                                                       Target => Outcome_Client_P);
         Producer_Job_S : constant Cancelation_Job := (Waiting_Microseconds => Busy_Waiting_Microseconds,
                                                       Target => Outcome_Client_S);

         Consumer_Count : constant := 50_000;

         Test_Outcome : Outcome_Client;

         Start_Time, Outcome_Init_Time, Schedule_Time, When_All_Time, Wait_Over_Time, End_Time : Ada.Calendar.Time;
         Parallel_Duration, Sequential_Duration : Duration;

         procedure Run_Serial_Tests (On : in out Executor) is
         begin
            -- Run the serial tests on the selected executor
            Start_Time := Ada.Calendar.Clock;
            for I in 1 .. Consumer_Count loop
               Test_Outcome := On.Schedule_Job (Consumer_Job_S, Test_Outcome);
            end loop;
            Test_Outcome := On.Schedule_Job (Producer_Job_S, Test_Outcome);
            Schedule_Time := Ada.Calendar.Clock;
            begin
               Test_Outcome.Wait_Completion;
            exception
               when Outcomes.Interfaces.Operation_Canceled =>
                  null;
            end;
            End_Time := Ada.Calendar.Clock;

            -- Analyze sequential results
            Sequential_Duration := End_Time - Start_Time;
            Ada.Text_IO.Put_Line ("Serial run on parallel executor took " & Duration'Image (Sequential_Duration) &
                                    " s (" & Duration'Image (Sequential_Duration / Parallel_Duration) &
                                    "x parallel run):");
            Ada.Text_IO.Put_Line ("   - Scheduling took " & Duration'Image (Schedule_Time - Start_Time) & " s.");
            Ada.Text_IO.Put_Line ("   - Synchronization took " & Duration'Image (End_Time - Schedule_Time) & " s.");
         end Run_Serial_Tests;

      begin

         -- Parallel version
         Ada.Text_IO.Put_Line ("=== Testing the executor's custom wait performance ===");
         Start_Time := Ada.Calendar.Clock;
         declare
            use type Outcome_Composition.Interfaces.Outcome_List;
            Parallel_Outcomes : Outcome_Composition.Interfaces.Outcome_List (1 .. Consumer_Count);
            Producer_Outcome : Outcome_Client;
         begin
            Outcome_Init_Time := Ada.Calendar.Clock;
            Parallel_Outcomes := (others => Parallel_Executor.Schedule_Job (Consumer_Job_P));
            Producer_Outcome := Parallel_Executor.Schedule_Job (Producer_Job_P);
            Schedule_Time := Ada.Calendar.Clock;
            Test_Outcome := Outcome_Composition.Shorthands.When_All (Parallel_Outcomes & Producer_Outcome);
            When_All_Time := Ada.Calendar.Clock;
            begin
               Test_Outcome.Wait_Completion;
            exception
               when Outcomes.Interfaces.Operation_Canceled =>
                  null;
            end;
            Wait_Over_Time := Ada.Calendar.Clock;
         end;
         End_Time := Ada.Calendar.Clock;

         -- Analyze parallel results
         Parallel_Duration := End_Time - Start_Time;
         Ada.Text_IO.Put_Line ("Parallel run took " & Duration'Image (Parallel_Duration) & " s:");
         Ada.Text_IO.Put_Line ("   - Outcome creation took " & Duration'Image (Outcome_Init_Time - Start_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Scheduling took " & Duration'Image (Schedule_Time - Outcome_Init_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Composition took " & Duration'Image (When_All_Time - Schedule_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Synchronization took " & Duration'Image (Wait_Over_Time - When_All_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Outcome liberation took " & Duration'Image (End_Time - Wait_Over_Time) & " s.");

         -- Run serial tests
         Run_Serial_Tests (Parallel_Executor);
         Run_Serial_Tests (Serial_Executor);

         -- Estimate scheduling overhead
         declare
            Scheduling_Overhead : constant Float := Float (Sequential_Duration) / Float (Consumer_Count + 1);
            type Rounded_Overhead is delta 0.001 range -1000.0 .. 1000.0;
            Overhead_In_Microseconds : constant Rounded_Overhead := Rounded_Overhead (Scheduling_Overhead * 10.0**6);
         begin
            Ada.Text_IO.Put_Line ("The overhead of custom waits is about " &
                                    Rounded_Overhead'Image (Overhead_In_Microseconds) & " microseconds");
         end;
         Ada.Text_IO.New_Line;

      end Benchmark_Wait_Custom;

   begin
      Benchmark_Null_Job;
      Benchmark_Yielding;
      Benchmark_Waiting;
      Benchmark_Wait_Ready;
      Benchmark_Wait_Custom;
   end Run_Benchmarks;

end Phalanstery.Examples.Scheduler_Benchmarks;
