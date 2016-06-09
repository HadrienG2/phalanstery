package Asynchronous.Tasks.Trivial is

   -- This package defines some trivial examples of asynchronous tasks, which may be used in unit tests or and
   -- executor benchmarks.

   -- Anytime one of these trivial tasks voluntarily raises an exception, it will be this one
   Expected_Error : exception;

   -- This task finishes immediately
   type Null_Task is new Tasks.Async_Task with null record;
   overriding function Run (Who : in out Null_Task) return Tasks.Return_Value;

   -- This task yields in a tight loop
   type Yielding_Task (Iterations : Natural) is new Tasks.Async_Task with
      record
         Counter : Natural := 0;
      end record;
   overriding function Run (Who : in out Yielding_Task) return Tasks.Return_Value;

   -- This task aborts with an exception
   type Erronerous_Task is new Tasks.Async_Task with null record;
   overriding function Run (Who : in out Erronerous_Task) return Tasks.Return_Value;

   -- This task waits for a finished event, then finishes
   type Ready_Wait_Task is new Tasks.Async_Task with
      record
         Has_Waited : Boolean := False;
      end record;
   overriding function Run (Who : in out Ready_Wait_Task) return Tasks.Return_Value;

   -- This task waits for a canceled event, and will never finish
   type Canceled_Wait_Task is new Tasks.Async_Task with null record;
   overriding function Run (Who : in out Canceled_Wait_Task) return Tasks.Return_Value;

   -- This task waits for an erronerous event, and will never finish
   type Error_Wait_Task is new Tasks.Async_Task with null record;
   overriding function Run (Who : in out Error_Wait_Task) return Tasks.Return_Value;

   -- This task waits for an arbitrary event, to be triggered by another task
   type Custom_Wait_Task is new Tasks.Async_Task with
      record
         Target : Valid_Event_Client;
      end record;
   overriding function Run (Who : in out Custom_Wait_Task) return Tasks.Return_Value;

   -- This task cancels an event and returns, it may be used in conjunction with the previous task
   type Event_Cancelation_Task is new Tasks.Async_Task with
      record
         Target : Valid_Event_Client;
      end record;
   overriding function Run (Who : in out Event_Cancelation_Task) return Tasks.Return_Value;

end Asynchronous.Tasks.Trivial;
