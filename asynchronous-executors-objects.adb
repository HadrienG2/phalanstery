with Ada.Unchecked_Deallocation;

package body Asynchronous.Executors.Objects is

   overriding procedure Schedule_Task (Where : in out Executor;
                                       What : Interfaces.Any_Async_Task) is
      Unused : constant Interfaces.Event_Client := Schedule_Task (Where => Where,
                                                                  What  => What) with Unreferenced;
   begin
      null;
   end Schedule_Task;

   overriding procedure Schedule_Task (Where : in out Executor;
                                       What  : Interfaces.Any_Async_Task;
                                       After : Interfaces.Event_Client) is
      Unused : constant Interfaces.Event_Client := Schedule_Task (Where => Where,
                                                                  What  => What,
                                                                  After => After) with Unreferenced;
   begin
      null;
   end Schedule_Task;

   overriding procedure Schedule_Task (Where : in out Executor;
                                       What  : Interfaces.Any_Async_Task;
                                       After : Interfaces.Event_Wait_List) is
      Unused : constant Interfaces.Event_Client := Schedule_Task (Where => Where,
                                                                  What  => What,
                                                                  After => After) with Unreferenced;
   begin
      null;
   end Schedule_Task;

   overriding function Schedule_Task (Where : in out Executor;
                                      What : Interfaces.Any_Async_Task) return Interfaces.Event_Client is
      Empty_Wait_List : Interfaces.Event_Wait_List (2 .. 1);
   begin
      return Schedule_Task (Where => Where,
                            What  => What,
                            After => Empty_Wait_List);
   end Schedule_Task;

   overriding function Schedule_Task (Where : in out Executor;
                           What  : Interfaces.Any_Async_Task;
                           After : Interfaces.Event_Client) return Interfaces.Event_Client is
   begin
      return Schedule_Task (Where => Where,
                            What  => What,
                            After => (1 => After));
   end Schedule_Task;

   overriding function Schedule_Task (Where : in out Executor;
                           What  : Interfaces.Any_Async_Task;
                           After : Interfaces.Event_Wait_List) return Interfaces.Event_Client is
   begin
      return Result : Interfaces.Event_Client do
         Where.Executor_Task.Schedule_Task (What  => What,
                                            After => After,
                                            Event => Result);
      end return;
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

end Asynchronous.Executors.Objects;
