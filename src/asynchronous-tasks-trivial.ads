package Asynchronous.Tasks.Trivial is

   -- This package defines some trivial examples of asynchronous tasks, which may be used in unit tests or as a
   -- component of executor performance benchmarks.

   -- Anytime one of these trivial tasks voluntarily raises an exception, it will be this one
   Expected_Error : exception;

   -- This task finishes immediately, it is useful for examining lock contention overhead
   type Null_Task is new Async_Task with null record;
   overriding function Run (Who          : in out Null_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task yields in a tight loop
   type Yielding_Task (Iterations : Natural) is new Async_Task with
      record
         Counter : Natural := 0;
      end record;
   overriding function Run (Who          : in out Yielding_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task, like the following ones, burns CPU cycles in a tight loop before returning.
   -- This ensures that we can test scheduling overhead without lock contention
   type Waiting_Task (Waiting_Nanoseconds : Natural) is new Async_Task with null record;
   overriding function Run (Who          : in out Waiting_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task aborts with an exception
   type Erronerous_Task (Waiting_Nanoseconds : Natural) is new Async_Task with null record;
   overriding function Run (Who          : in out Erronerous_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task waits for a finished event, then finishes
   type Ready_Wait_Task (Waiting_Nanoseconds : Natural) is new Async_Task with
      record
         Has_Waited : Boolean := False;
      end record;
   overriding function Run (Who          : in out Ready_Wait_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task waits for a canceled event, and will never finish
   type Canceled_Wait_Task (Waiting_Nanoseconds : Natural) is new Async_Task with null record;
   overriding function Run (Who          : in out Canceled_Wait_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task waits for an erronerous event, and will never finish
   type Error_Wait_Task (Waiting_Nanoseconds : Natural) is new Async_Task with null record;
   overriding function Run (Who          : in out Error_Wait_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task waits for an arbitrary event, to be triggered by another task
   type Custom_Wait_Task (Waiting_Nanoseconds : Natural) is new Async_Task with
      record
         Target : Valid_Event_Client;
      end record;
   overriding function Run (Who          : in out Custom_Wait_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

   -- This task cancels an event and returns. It may be used in conjunction with the previous task
   type Event_Cancelation_Task (Waiting_Nanoseconds : Natural) is new Async_Task with
      record
         Target : Valid_Event_Client;
      end record;
   overriding function Run (Who          : in out Event_Cancelation_Task;
                            Was_Canceled : Boolean) return Tasks.Return_Value;

private

   procedure Busy_Wait (Nanoseconds : Natural);

end Asynchronous.Tasks.Trivial;
