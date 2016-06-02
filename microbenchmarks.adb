with Ada.Calendar;
with Asynchronous.Executors.Interfaces;
with Asynchronous.Executors.Objects;
with Ada.Text_IO;
with Events.Composition.Shortcuts;
with Events.Interfaces;
with Events.Servers;
with System.Multiprocessors;
pragma Elaborate_All (Events.Servers);

package body Microbenchmarks is

   Ready_Event, Canceled_Event, Error_Event : Events.Clients.Client;

   overriding function Run (Who : in out Null_Task) return Async_Tasks.Return_Value is (Async_Tasks.Return_Finished);

   overriding function Run (Who : in out Yielding_Task) return Async_Tasks.Return_Value is
   begin
      if Who.Counter < Who.Iterations then
         -- Keep the CPU busy for a while in the most inefficient way we can think of
         Who.Counter := Who.Counter + 1;

         -- Tell the scheduler that we still have work to do
         return Async_Tasks.Return_Yielding;
      else
         -- Mark the task as finished
         return Async_Tasks.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who : in out Ready_Wait_Task) return Async_Tasks.Return_Value is
   begin
      if not Who.Has_Waited then
         Who.Has_Waited := True;
         return Async_Tasks.Return_Waiting (Ready_Event);
      else
         return Async_Tasks.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who : in out Canceled_Wait_Task) return Async_Tasks.Return_Value is
     (Async_Tasks.Return_Waiting (Canceled_Event));

   overriding function Run (Who : in out Error_Wait_Task) return Async_Tasks.Return_Value is
     (Async_Tasks.Return_Waiting (Error_Event));

   overriding function Run (Who : in out Custom_Wait_Task) return Async_Tasks.Return_Value is
     (Async_Tasks.Return_Waiting (Who.Target));

   overriding function Run (Who : in out Wait_Cancelation_Task) return Async_Tasks.Return_Value is
   begin
      Who.Target.Cancel;
      return Async_Tasks.Return_Finished;
   end Run;

   procedure Run_Benchmarks is

      use type Ada.Calendar.Time;

      Default_Executor : Asynchronous.Executors.Objects.Executor;
      Number_Of_CPUs : constant Positive := Positive (System.Multiprocessors.Number_Of_CPUs);

      Final_Status : Events.Interfaces.Event_Status with Unreferenced;

      procedure Benchmark_Task (What                : Asynchronous.Executors.Interfaces.Any_Async_Task;
                                How_Many            : Positive;
                                Title               : String;
                                Feature_Name        : String;
                                Internal_Iterations : Positive := 1) is

         Test_Event : Events.Clients.Client;

         Start_Time : Ada.Calendar.Time;
         Parallel_Duration, Sequential_Duration, Direct_Run_Duration : Duration;

      begin

         -- Parallel version
         Ada.Text_IO.Put_Line ("=== Testing the executor's " & Title & " performance ===");
         Start_Time := Ada.Calendar.Clock;
         declare
            Parallel_Events : constant Events.Composition.Event_List (1 .. How_Many) :=
              (others => Default_Executor.Schedule_Task (What));
         begin
            Test_Event := Events.Composition.Shortcuts.When_All (Parallel_Events);
         end;
         Test_Event.Wait_Completion (Final_Status);
         Parallel_Duration := Ada.Calendar.Clock - Start_Time;
         Ada.Text_IO.Put_Line ("Parallel run took " & Duration'Image (Parallel_Duration) & " s");

         -- Serial version
         Start_Time := Ada.Calendar.Clock;
         for I in 1 .. How_Many loop
            Test_Event := Default_Executor.Schedule_Task (What, Test_Event);
         end loop;
         Test_Event.Wait_Completion (Final_Status);
         Sequential_Duration := Ada.Calendar.Clock - Start_Time;
         Ada.Text_IO.Put_Line ("Serial run took " & Duration'Image (Sequential_Duration) & " s (" &
                                 Duration'Image (Sequential_Duration / Parallel_Duration) & "x slower than parallel)");

         -- Direct run (no executor)
         Start_Time := Ada.Calendar.Clock;
         for I in 1 .. How_Many loop
            declare
               Task_Copy : Asynchronous.Executors.Interfaces.Any_Async_Task := What;
            begin
               declare
                  use type Async_Tasks.Return_Status;
                  Result : constant Async_Tasks.Return_Value := Task_Copy.Run;
               begin
                  exit when Async_Tasks.Status (Result) /= Async_Tasks.Yielding;
               end;
            end;
         end loop;
         Test_Event.Wait_Completion (Final_Status);
         Direct_Run_Duration := Ada.Calendar.Clock - Start_Time;
         Ada.Text_IO.Put_Line ("Direct run took " & Duration'Image (Direct_Run_Duration) & " s (" &
                                 Duration'Image (Sequential_Duration/Direct_Run_Duration) & "x faster than serial async)");

         -- Estimate scheduling overhead
         declare
            Total_Scheduling_Duration : constant Duration := Sequential_Duration - Direct_Run_Duration;
            Scheduling_Overhead : constant Float := Float (Total_Scheduling_Duration) / (Float (How_Many * Internal_Iterations));
            type Rounded_Overhead is delta 0.01 range 0.0 .. 100.0;
            Overhead_In_Microseconds : constant Rounded_Overhead := Rounded_Overhead (Scheduling_Overhead * 10.0**6);
         begin
            Ada.Text_IO.Put_Line ("The overhead of " & Feature_Name & " is about " &
                                    Rounded_Overhead'Image (Overhead_In_Microseconds) & " microseconds");
         end;
         Ada.Text_IO.New_Line;

      end Benchmark_Task;

      procedure Benchmark_Startup is
         My_Task : Null_Task;
      begin
         Benchmark_Task (What         => My_Task,
                         How_Many     => 150_000,
                         Title        => "startup",
                         Feature_Name => "starting a task");
      end Benchmark_Startup;

      procedure Benchmark_Yielding is
         Yielding_Iterations : constant := 50_000;
         My_Task : Yielding_Task (Yielding_Iterations);
      begin

         Benchmark_Task (What                => My_Task,
                         How_Many            => 2*Number_Of_CPUs,
                         Title               => "yielding",
                         Feature_Name        => "yielding in a task",
                         Internal_Iterations => Yielding_Iterations);
      end Benchmark_Yielding;

      procedure Benchmark_Wait_Ready is
         My_Task : Ready_Wait_Task;
      begin
         Benchmark_Task (What         => My_Task,
                         How_Many     => 150_000,
                         Title        => "nonblocking wait",
                         Feature_Name => "waiting for a ready event");
      end Benchmark_Wait_Ready;

      procedure Benchmark_Wait_Canceled is
         My_Task : Canceled_Wait_Task;
      begin
         Benchmark_Task (What         => My_Task,
                         How_Many     => 150_000,
                         Title        => "canceled wait",
                         Feature_Name => "waiting for a canceled event");
      end Benchmark_Wait_Canceled;

      procedure Benchmark_Wait_Custom is

         Event_Server_P, Event_Server_S : Events.Servers.Server;
         Event_Client_P : constant Events.Clients.Client := Event_Server_P.Make_Client;
         Event_Client_S : constant Events.Clients.Client := Event_Server_S.Make_Client;
         Consumer_Task_P : constant Custom_Wait_Task := (Target => Event_Client_P);
         Consumer_Task_S : constant Custom_Wait_Task := (Target => Event_Client_S);
         Producer_Task_P : constant Wait_Cancelation_Task := (Target => Event_Client_P);
         Producer_Task_S : constant Wait_Cancelation_Task := (Target => Event_Client_S);

         Consumer_Count : constant := 1_000;

         Test_Event : Events.Clients.Client;

         Start_Time : Ada.Calendar.Time;
         Parallel_Duration, Sequential_Duration : Duration;

      begin

         -- Parallel version
         Ada.Text_IO.Put_Line ("=== Testing the executor's custom wait performance ===");
         Start_Time := Ada.Calendar.Clock;
         declare
            use type Events.Composition.Event_List;
            Parallel_Events : constant Events.Composition.Event_List (1 .. Consumer_Count) :=
              (others => Default_Executor.Schedule_Task (Consumer_Task_P));
            Producer_Event : constant Events.Clients.Client := Default_Executor.Schedule_Task (Producer_Task_P);
         begin
            Test_Event := Events.Composition.Shortcuts.When_All (Parallel_Events & Producer_Event);
         end;
         Test_Event.Wait_Completion (Final_Status);
         Parallel_Duration := Ada.Calendar.Clock - Start_Time;
         Ada.Text_IO.Put_Line ("Parallel run took " & Duration'Image (Parallel_Duration) & " s");

         -- Serial version
         Start_Time := Ada.Calendar.Clock;
         for I in 1 .. Consumer_Count loop
            Test_Event := Default_Executor.Schedule_Task (Consumer_Task_S, Test_Event);
         end loop;
         Test_Event := Default_Executor.Schedule_Task (Producer_Task_S, Test_Event);
         Test_Event.Wait_Completion (Final_Status);
         Sequential_Duration := Ada.Calendar.Clock - Start_Time;
         Ada.Text_IO.Put_Line ("Serial run took " & Duration'Image (Sequential_Duration) & " s (" &
                                 Duration'Image (Sequential_Duration / Parallel_Duration) & "x slower than parallel)");

         -- Estimate scheduling overhead
         declare
            Scheduling_Overhead : constant Float := Float (Sequential_Duration) / Float (Consumer_Count+1);
            type Rounded_Overhead is delta 0.01 range 0.0 .. 100.0;
            Overhead_In_Microseconds : constant Rounded_Overhead := Rounded_Overhead (Scheduling_Overhead * 10.0**6);
         begin
            Ada.Text_IO.Put_Line ("The overhead of custom waits is about " &
                                    Rounded_Overhead'Image (Overhead_In_Microseconds) & " microseconds");
         end;
         Ada.Text_IO.New_Line;

      end Benchmark_Wait_Custom;

   begin
--        Benchmark_Startup;
--        Benchmark_Yielding;
--        Benchmark_Wait_Ready;
--        Benchmark_Wait_Canceled;
      Benchmark_Wait_Custom;
   end Run_Benchmarks;

begin

   declare
      S : Events.Servers.Server;
   begin
      S.Mark_Done;
      Ready_Event := S.Make_Client;
   end;

   declare
      S : Events.Servers.Server;
   begin
      S.Cancel;
      Canceled_Event := S.Make_Client;
   end;

   declare
      S : Events.Servers.Server;
      Custom_Error : exception;
   begin
      begin
         raise Custom_Error;
      exception
         when E : Custom_Error =>
            S.Mark_Error (E);
      end;
   end;

end Microbenchmarks;
