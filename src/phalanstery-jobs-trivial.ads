package Phalanstery.Jobs.Trivial is

   -- This package defines some trivial examples of asynchronous jobs, which may be used in unit tests or as a
   -- component of executor performance benchmarks.

   -- Anytime one of these trivial jobs voluntarily raises an exception, it will be this one
   Expected_Error : exception;

   -- This job finishes immediately, it is useful for examining lock contention overhead
   type Null_Job is new Async_Job with null record;
   overriding function Run (Who          : in out Null_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job yields in a tight loop
   type Yielding_Job (Iterations : Natural) is new Async_Job with
      record
         Counter : Natural := 0;
      end record;
   overriding function Run (Who          : in out Yielding_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job, like the following ones, burns CPU cycles in a tight loop before returning.
   -- This ensures that we can test scheduling overhead without lock contention
   type Waiting_Job (Waiting_Nanoseconds : Natural) is new Async_Job with null record;
   overriding function Run (Who          : in out Waiting_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job aborts with an exception
   type Erronerous_Job (Waiting_Nanoseconds : Natural) is new Async_Job with null record;
   overriding function Run (Who          : in out Erronerous_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job waits for a finished event, then finishes
   type Ready_Wait_Job (Waiting_Nanoseconds : Natural) is new Async_Job with
      record
         Has_Waited : Boolean := False;
      end record;
   overriding function Run (Who          : in out Ready_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job waits for a canceled event, and will never finish
   type Canceled_Wait_Job (Waiting_Nanoseconds : Natural) is new Async_Job with null record;
   overriding function Run (Who          : in out Canceled_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job waits for an erronerous event, and will never finish
   type Error_Wait_Job (Waiting_Nanoseconds : Natural) is new Async_Job with null record;
   overriding function Run (Who          : in out Error_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job waits for an arbitrary event, to be triggered by another job
   type Custom_Wait_Job (Waiting_Nanoseconds : Natural) is new Async_Job with
      record
         Target : Valid_Event_Client;
      end record;
   overriding function Run (Who          : in out Custom_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

   -- This job cancels an event and returns. It may be used in conjunction with the previous job
   type Event_Cancelation_Job (Waiting_Nanoseconds : Natural) is new Async_Job with
      record
         Target : Valid_Event_Client;
      end record;
   overriding function Run (Who          : in out Event_Cancelation_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value;

private

   procedure Busy_Wait (Nanoseconds : Natural);

end Phalanstery.Jobs.Trivial;
