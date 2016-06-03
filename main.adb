pragma Warnings (Off);
with Asynchronous;
with Asynchronous.Events;
with Asynchronous.Events.Callbacks;
with Asynchronous.Events.Clients;
with Asynchronous.Events.Composition;
with Asynchronous.Events.Composition.And_Gates;
with Asynchronous.Events.Composition.Shortcuts;
with Asynchronous.Events.Implementation;
with Asynchronous.Events.Interfaces;
with Asynchronous.Events.Servers;
with Asynchronous.Executors;
with Asynchronous.Executors.Implementation;
with Asynchronous.Executors.Interfaces;
with Asynchronous.Executors.Objects;
with Asynchronous.Tasks;
with Asynchronous.Utilities;
with Asynchronous.Utilities.Atomic_Counters;
with Asynchronous.Utilities.Barriers;
with Asynchronous.Utilities.Debug;
with Asynchronous.Utilities.References;
with Asynchronous.Utilities.References.Not_Null;
with Asynchronous.Utilities.References.Nullable;
with Asynchronous.Utilities.Signals;
with Asynchronous.Utilities.Testing;
pragma Warnings (On);

with Microbenchmarks;

procedure Main is

   -- The basic idea here is to devise an event-driven model of asynchronous computation, so that a correct program
   -- only needs to block an OS thread for waiting exactly once, when fetching the final result of a computation.
   --
   -- Motto : "Blocking more than once in a program is a bug in the program, the hardware, or the API"

   -- === INPUT AND OUTPUT ===

   -- Passing initial input to an asynchronous task is best done using a task-specific constructor.
   -- But there should be a way to schedule running a task based on input that is not available yet, like hpx::dataflow does.

   -- To exchange data with the outside world on every task iteration, one may use input and output queues called channels.
   -- Senders and listeners would be notified of exceptions (and thus avoid hanging indefinitely) through channel RAII.
   -- A channel input may be associated to multiple outputs, but a misbehaving listener shall not be able to stop the task.
   -- Ideally, a container iterator should be able to behave as a channel, or be easily converted into it, for in-place data access.

   -- If a notion of a final result is required, it is best represented using a future-like object. HPX' futures may be used for inspiration.

   -- TODO : Determine a way to pass data in and out of tasks at start, end, and on every iteration
   -- TODO : Determine a composable dataflow model for synchronizing with a task's inputs and outputs
   -- TODO : Add continuations ?
   -- TODO : Think about composition of iterators and data !
   -- TODO : Add data-parallel programming facilities

begin

   -- null; -- Remove scheduler benchmark for coverage tests
   Microbenchmarks.Run_Benchmarks;

exception
   when E : others =>
      Asynchronous.Utilities.Debug.Display_Unhandled_Exception ("the main program", E);
      raise;
end Main;
