with Ada.Calendar;
with Asynchronous.Executors.Interfaces;
with Asynchronous.Executors.Objects;
with Asynchronous.Events.Clients;
with Asynchronous.Events.Composition.Shortcuts;
with Asynchronous.Events.Contracts;
with Asynchronous.Events.Interfaces;
with Asynchronous.Events.Servers;
with Asynchronous.Tasks.Trivial;
with Ada.Text_IO;
with System.Multiprocessors;
pragma Elaborate_All (Asynchronous.Events.Servers);

package body Benchmarks is

   procedure Run_Benchmarks is

      use type Ada.Calendar.Time;
      package Async_Tasks renames Asynchronous.Tasks;
      subtype Event_Client is Asynchronous.Events.Clients.Client;

      Parallel_Executor : Asynchronous.Executors.Objects.Executor;
      Serial_Executor : Asynchronous.Executors.Objects.Executor (1);
      Number_Of_CPUs : constant Positive := Positive (System.Multiprocessors.Number_Of_CPUs);

      procedure Benchmark_Task (What                : Asynchronous.Executors.Interfaces.Any_Async_Task;
                                How_Many            : Positive;
                                Title               : String;
                                Feature_Name        : String;
                                Internal_Iterations : Positive := 1) is

         Test_Event : Event_Client;

         Start_Time, Event_Init_Time, Schedule_Time, When_All_Time, Wait_Over_Time, End_Time : Ada.Calendar.Time;
         Parallel_Duration, Sequential_Duration, Direct_Run_Duration : Duration;

         procedure Run_Serial_Tests (On : in out Asynchronous.Executors.Objects.Executor;
                                     Executor_Kind : String) is
         begin
            -- Run the serial tests
            Start_Time := Ada.Calendar.Clock;
            for I in 1 .. How_Many loop
               Test_Event := On.Schedule_Task (What, Test_Event);
            end loop;
            Schedule_Time := Ada.Calendar.Clock;
            Test_Event.Wait_Completion;
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
            Parallel_Events : Asynchronous.Events.Composition.Nullable_Event_List (1 .. How_Many);
         begin
            Event_Init_Time := Ada.Calendar.Clock;
            Parallel_Events := (others => Parallel_Executor.Schedule_Task (What));
            Schedule_Time := Ada.Calendar.Clock;
            Test_Event := Asynchronous.Events.Composition.Shortcuts.When_All (Parallel_Events);
            When_All_Time := Ada.Calendar.Clock;
            Test_Event.Wait_Completion;
            Wait_Over_Time := Ada.Calendar.Clock;
         end;
         End_Time := Ada.Calendar.Clock;

         -- Analyze parallel results
         Parallel_Duration := End_Time - Start_Time;
         Ada.Text_IO.Put_Line ("Parallel run took " & Duration'Image (Parallel_Duration) & " s:");
         Ada.Text_IO.Put_Line ("   - Event creation took " & Duration'Image (Event_Init_Time - Start_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Scheduling took " & Duration'Image (Schedule_Time - Event_Init_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Composition took " & Duration'Image (When_All_Time - Schedule_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Synchronization took " & Duration'Image (Wait_Over_Time - When_All_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Event liberation took " & Duration'Image (End_Time - Wait_Over_Time) & " s.");

         -- Serial version
         Run_Serial_Tests (Parallel_Executor, "parallel");
         Run_Serial_Tests (Serial_Executor, "serial");

         -- Direct run (no executor)
         Start_Time := Ada.Calendar.Clock;
         Task_Loop :
         for I in 1 .. How_Many loop
            declare
               Task_Copy : Asynchronous.Executors.Interfaces.Any_Async_Task := What;
            begin
               Direct_Execution_Loop :
               loop
                  declare
                     use all type Async_Tasks.Return_Status;
                     Result : constant Async_Tasks.Return_Value := Task_Copy.Run (Was_Canceled => False);
                  begin
                     case Async_Tasks.Status (Result) is
                        when Finished =>
                           exit Direct_Execution_Loop;
                        when Yielding =>
                           null;
                        when Canceled =>
                           exit Task_Loop;
                        when Waiting =>
                           declare
                              use all type Asynchronous.Events.Interfaces.Event_Status;
                              E : constant Asynchronous.Events.Contracts.Valid_Event_Client :=
                                Asynchronous.Events.Composition.Shortcuts.When_All (Async_Tasks.Wait_List (Result));
                           begin
                              case E.Status is
                                 when Pending =>
                                    raise Constraint_Error with "Direct runs cannot handle truly blocking tasks";
                                 when Done =>
                                    null;
                                 when Canceled | Error =>
                                    exit Task_Loop;
                              end case;
                           end;
                     end case;
                  end;
               end loop Direct_Execution_Loop;
            end;
         end loop Task_Loop;
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

      end Benchmark_Task;

      procedure Benchmark_Null_Task is
         My_Task : Async_Tasks.Trivial.Null_Task;
      begin
         Benchmark_Task (What         => My_Task,
                         How_Many     => 150_000,
                         Title        => "null task",
                         Feature_Name => "running the null task");
      end Benchmark_Null_Task;

      procedure Benchmark_Yielding is
         Yielding_Iterations : constant := 50_000;
         My_Task : Async_Tasks.Trivial.Yielding_Task (Yielding_Iterations);
      begin

         Benchmark_Task (What                => My_Task,
                         How_Many            => 2 * Number_Of_CPUs,
                         Title               => "yielding",
                         Feature_Name        => "yielding in a task",
                         Internal_Iterations => Yielding_Iterations);
      end Benchmark_Yielding;

      Busy_Waiting_Nanoseconds : constant := 100_000;

      procedure Benchmark_Waiting is
         My_Task : Async_Tasks.Trivial.Waiting_Task (Busy_Waiting_Nanoseconds);
      begin

         Benchmark_Task (What                => My_Task,
                         How_Many            => 50_000,
                         Title               => "busy-waiting",
                         Feature_Name        => "running a busy-waiting task");
      end Benchmark_Waiting;

      procedure Benchmark_Wait_Ready is
         My_Task : Async_Tasks.Trivial.Ready_Wait_Task (Busy_Waiting_Nanoseconds);
      begin
         Benchmark_Task (What         => My_Task,
                         How_Many     => 50_000,
                         Title        => "ready wait",
                         Feature_Name => "waiting for a ready event");
      end Benchmark_Wait_Ready;

      procedure Benchmark_Wait_Custom is

         Event_Server_S : constant Asynchronous.Events.Servers.Server := Asynchronous.Events.Servers.Make_Event;
         Event_Server_P : constant Asynchronous.Events.Servers.Server := Asynchronous.Events.Servers.Make_Event;
         Event_Client_P : constant Event_Client := Event_Server_P.Make_Client;
         Event_Client_S : constant Event_Client := Event_Server_S.Make_Client;

         subtype Custom_Wait_Task is Async_Tasks.Trivial.Custom_Wait_Task;
         subtype Event_Cancelation_Task is Async_Tasks.Trivial.Event_Cancelation_Task;
         Consumer_Task_P : constant Custom_Wait_Task := (Waiting_Nanoseconds => Busy_Waiting_Nanoseconds,
                                                        Target => Event_Client_P);
         Consumer_Task_S : constant Custom_Wait_Task := (Waiting_Nanoseconds => Busy_Waiting_Nanoseconds,
                                                         Target => Event_Client_S);
         Producer_Task_P : constant Event_Cancelation_Task := (Waiting_Nanoseconds => Busy_Waiting_Nanoseconds,
                                                               Target => Event_Client_P);
         Producer_Task_S : constant Event_Cancelation_Task := (Waiting_Nanoseconds => Busy_Waiting_Nanoseconds,
                                                               Target => Event_Client_S);

         Consumer_Count : constant := 50_000;

         Test_Event : Event_Client;

         Start_Time, Event_Init_Time, Schedule_Time, When_All_Time, Wait_Over_Time, End_Time : Ada.Calendar.Time;
         Parallel_Duration, Sequential_Duration : Duration;

         procedure Run_Serial_Tests (On : in out Asynchronous.Executors.Objects.Executor) is
         begin
            -- Run the serial tests on the selected executor
            Start_Time := Ada.Calendar.Clock;
            for I in 1 .. Consumer_Count loop
               Test_Event := On.Schedule_Task (Consumer_Task_S, Test_Event);
            end loop;
            Test_Event := On.Schedule_Task (Producer_Task_S, Test_Event);
            Schedule_Time := Ada.Calendar.Clock;
            begin
               Test_Event.Wait_Completion;
            exception
               when Asynchronous.Events.Interfaces.Event_Canceled =>
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
            use type Asynchronous.Events.Composition.Nullable_Event_List;
            Parallel_Events : Asynchronous.Events.Composition.Nullable_Event_List (1 .. Consumer_Count);
            Producer_Event : Event_Client;
         begin
            Event_Init_Time := Ada.Calendar.Clock;
            Parallel_Events := (others => Parallel_Executor.Schedule_Task (Consumer_Task_P));
            Producer_Event := Parallel_Executor.Schedule_Task (Producer_Task_P);
            Schedule_Time := Ada.Calendar.Clock;
            Test_Event := Asynchronous.Events.Composition.Shortcuts.When_All (Parallel_Events & Producer_Event);
            When_All_Time := Ada.Calendar.Clock;
            begin
               Test_Event.Wait_Completion;
            exception
               when Asynchronous.Events.Interfaces.Event_Canceled =>
                  null;
            end;
            Wait_Over_Time := Ada.Calendar.Clock;
         end;
         End_Time := Ada.Calendar.Clock;

         -- Analyze parallel results
         Parallel_Duration := End_Time - Start_Time;
         Ada.Text_IO.Put_Line ("Parallel run took " & Duration'Image (Parallel_Duration) & " s:");
         Ada.Text_IO.Put_Line ("   - Event creation took " & Duration'Image (Event_Init_Time - Start_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Scheduling took " & Duration'Image (Schedule_Time - Event_Init_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Composition took " & Duration'Image (When_All_Time - Schedule_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Synchronization took " & Duration'Image (Wait_Over_Time - When_All_Time) & " s.");
         Ada.Text_IO.Put_Line ("   - Event liberation took " & Duration'Image (End_Time - Wait_Over_Time) & " s.");

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
      Benchmark_Null_Task;
      Benchmark_Yielding;
      Benchmark_Waiting;
      Benchmark_Wait_Ready;
      Benchmark_Wait_Custom;
   end Run_Benchmarks;

end Benchmarks;
