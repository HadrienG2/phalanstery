with Ada.Unchecked_Deallocation;
with Asynchronous.Events.Clients;
with Asynchronous.Events.Contracts;
with Asynchronous.Events.Interfaces;
with Asynchronous.Events.Servers;
with Asynchronous.Tasks.Trivial;
with Asynchronous.Utilities.Testing;
with System.Multiprocessors;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Executors.Objects is

   overriding procedure Schedule_Task (Where : in out Executor;
                                       What : Interfaces.Any_Async_Task) is
      Unused : constant Interfaces.Valid_Event_Client := Schedule_Task (Where => Where,
                                                                        What  => What) with Unreferenced;
   begin
      null;
   end Schedule_Task;

   overriding procedure Schedule_Task (Where : in out Executor;
                                       What  : Interfaces.Any_Async_Task;
                                       After : Interfaces.Valid_Event_Client) is
      Unused : constant Interfaces.Valid_Event_Client := Schedule_Task (Where => Where,
                                                                        What  => What,
                                                                        After => After) with Unreferenced;
   begin
      null;
   end Schedule_Task;

   overriding procedure Schedule_Task (Where : in out Executor;
                                       What  : Interfaces.Any_Async_Task;
                                       After : Interfaces.Event_Wait_List) is
      Unused : constant Interfaces.Valid_Event_Client := Schedule_Task (Where => Where,
                                                                        What  => What,
                                                                        After => After) with Unreferenced;
   begin
      null;
   end Schedule_Task;

   overriding function Schedule_Task (Where : in out Executor;
                                      What : Interfaces.Any_Async_Task) return Interfaces.Valid_Event_Client is
      Empty_Wait_List : Interfaces.Event_Wait_List (2 .. 1);
   begin
      return Schedule_Task (Where => Where,
                            What  => What,
                            After => Empty_Wait_List);
   end Schedule_Task;

   overriding function Schedule_Task (Where : in out Executor;
                                      What  : Interfaces.Any_Async_Task;
                                      After : Interfaces.Valid_Event_Client) return Interfaces.Valid_Event_Client is
   begin
      return Schedule_Task (Where => Where,
                            What  => What,
                            After => (1 => After));
   end Schedule_Task;

   overriding function Schedule_Task (Where : in out Executor;
                                      What  : Interfaces.Any_Async_Task;
                                      After : Interfaces.Event_Wait_List) return Interfaces.Valid_Event_Client is
      Result : Events.Clients.Client;
   begin
      Where.Executor_Task.Schedule_Task (What  => What,
                                         After => After,
                                         Event => Result);
      return Result;
   end Schedule_Task;

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
      use all type Events.Interfaces.Finished_Event_Status;

      Number_Of_Workers : constant := 2;
      Test_Executor : Executor (Number_Of_Workers);
      T : Tasks.Trivial.Null_Task;

      procedure Test_Initial_State is
      begin
         Assert_Truth (Check   => (Test_Executor.Executor_Task /= null),
                       Message => "An executor task should be spawned on executor object creation");
         Assert_Truth (Check   => (Test_Executor.Executor_Task.Number_Of_Workers = Number_Of_Workers),
                       Message => "The executor task of an executor should have the right number of workers");
      end Test_Initial_State;

      procedure Test_Functions is
         Final_Status : Events.Interfaces.Finished_Event_Status;
         Dep1_S, Dep2_S : constant Events.Contracts.Valid_Event_Server := Events.Servers.Make_Event;
         Dep1_C : constant Events.Contracts.Valid_Event_Client := Dep1_S.Make_Client;
         Dep2_C : constant Events.Contracts.Valid_Event_Client := Dep2_S.Make_Client;
      begin

         declare
            C : constant Interfaces.Valid_Event_Client := Test_Executor.Schedule_Task (What => T);
         begin
            C.Wait_Completion (Final_Status);
            Assert_Truth (Check   => (Final_Status = Done),
                          Message => "The null task should complete properly after being scheduled");
         end;

         declare
            C : constant Interfaces.Valid_Event_Client := Test_Executor.Schedule_Task (What  => T,
                                                                                       After => Dep1_C);
         begin
            Assert_Truth (Check   => (C.Status = Pending),
                          Message => "The null task should not start until its dependencies are satisfied");

            Dep1_S.Mark_Done;
            C.Wait_Completion (Final_Status);
            Assert_Truth (Check   => (Final_Status = Done),
                          Message => "The null task should complete properly after its dependencies are met");
         end;

         declare
            C : constant Interfaces.Valid_Event_Client := Test_Executor.Schedule_Task (What  => T,
                                                                                       After => (Dep1_C, Dep2_C));
         begin
            Assert_Truth (Check   => (C.Status = Pending),
                          Message => "The null task should not start until all its dependencies are satisfied");

            Dep2_S.Mark_Done;
            C.Wait_Completion (Final_Status);
            Assert_Truth (Check   => (Final_Status = Done),
                          Message => "The null task should complete properly after its dependencies are met");
         end;

      end Test_Functions;

      procedure Test_Procedures is
         Alternate_Executor : Executor (Number_Of_Workers);
         S1, S2 : constant Events.Contracts.Valid_Event_Server := Events.Servers.Make_Event;
         C1 : constant Interfaces.Valid_Event_Client := S1.Make_Client;
         C2 : constant Interfaces.Valid_Event_Client := S2.Make_Client;
      begin
         -- Fire-and forget execution is particularly challenging to test, as we have no idea when it will occur and
         -- have no way to synchronize with it. Consequently, we only test that it does not hang or crash.
         Alternate_Executor.Schedule_Task (What => T);
         Alternate_Executor.Schedule_Task (What  => T,
                                           After => C1);
         Alternate_Executor.Schedule_Task (What  => T,
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

end Asynchronous.Executors.Objects;
