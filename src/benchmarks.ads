with Asynchronous.Tasks;
with Asynchronous.Events.Clients;
pragma Elaborate_All (Asynchronous.Events.Clients);

package Benchmarks is

   -- In this package, we implement a couple of very basic asynchronous tasks which provide scheduler benchmarks.
   package Async_Tasks renames Asynchronous.Tasks;
   subtype Event_Client is Asynchronous.Events.Clients.Client;

   -- This task finishes immediately
   type Null_Task is new Async_Tasks.Async_Task with null record;
   overriding function Run (Who : in out Null_Task) return Async_Tasks.Return_Value;

   -- This task yields in a tight loop
   type Yielding_Task (Iterations : Natural) is new Async_Tasks.Async_Task with
      record
         Counter : Natural := 0;
      end record;
   overriding function Run (Who : in out Yielding_Task) return Async_Tasks.Return_Value;

   -- This task waits for a finished event, then finishes
   type Ready_Wait_Task is new Async_Tasks.Async_Task with
      record
         Has_Waited : Boolean := False;
      end record;
   overriding function Run (Who : in out Ready_Wait_Task) return Async_Tasks.Return_Value;

   -- This task waits for a canceled event, and will never finish
   type Canceled_Wait_Task is new Async_Tasks.Async_Task with null record;
   overriding function Run (Who : in out Canceled_Wait_Task) return Async_Tasks.Return_Value;

   -- This task waits for an erronerous event, and will never finish
   type Error_Wait_Task is new Async_Tasks.Async_Task with null record;
   overriding function Run (Who : in out Error_Wait_Task) return Async_Tasks.Return_Value;

   -- This task waits for a pending event, that will be canceled by the next task
   type Custom_Wait_Task is new Async_Tasks.Async_Task with
      record
         Target : Event_Client;
      end record;
   overriding function Run (Who : in out Custom_Wait_Task) return Async_Tasks.Return_Value;

   -- This task cancels an event and returns
   type Wait_Cancelation_Task is new Async_Tasks.Async_Task with
      record
         Target : Event_Client;
      end record;
   overriding function Run (Who : in out Wait_Cancelation_Task) return Async_Tasks.Return_Value;


   -- This procedure runs scheduler benchmarks based on the tasks above
   procedure Run_Benchmarks;

end Benchmarks;
