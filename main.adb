pragma Warnings (Off);
with Asynchronous;
with Asynchronous.Tasks;
with Asynchronous.Executors;
with Asynchronous.Executors.Interfaces;
with Asynchronous.Executors.Implementation;
with Asynchronous.Executors.Objects;
with Events;
with Events.Callbacks;
with Events.Clients;
with Events.Composition;
with Events.Composition.And_Gates;
with Events.Composition.Shortcuts;
with Events.Implementation;
with Events.Interfaces;
with Events.Servers;
with Utilities;
with Utilities.Atomic_Counters;
with Utilities.Debug;
with Utilities.References;
with Utilities.References.Not_Null;
with Utilities.References.Nullable;
with Utilities.Signals;
with Utilities.Testing;
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
      Utilities.Debug.Display_Unhandled_Exception ("the main program", E);
      raise;
end Main;
