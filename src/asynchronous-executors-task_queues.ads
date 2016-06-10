with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Finalization;
with Asynchronous.Executors.Task_Instances.References;
with Asynchronous.Utilities.Atomic_Counters;

package Asynchronous.Executors.Task_Queues is

   -- Ready asynchronous tasks will be put on a FIFO queue, and executed once worker threads become available.
   subtype Task_Instance_Reference is Task_Instances.References.Reference;
   package Ready_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Task_Instance_Reference);
   package Ready_Queue_Implementation is new Ada.Containers.Unbounded_Synchronized_Queues (Ready_Queue_Interfaces);

   -- Pending tasks do not really follow an organized data structure: a task may become pending at any time, and resume
   -- execution at any other time, regardless of what any other task is doing.
   --
   -- For this reason, pending tasks will be stored on the heap (see Executors.Scheduling for more details), and the
   -- task queue will only keep track of their amount using an atomic conter.
   --
   type Pending_Counter is tagged limited private;

   procedure Add_Task (Where : in out Pending_Counter);

   procedure Remove_Task (Where : in out Pending_Counter);

   function No_Pending_Task (Where : Pending_Counter) return Boolean;


   -- A task queue is a combination of a ready task queue and a pending task counter. These two accounting facilities
   -- are kept separate and un-encapsulated so that a client may synchronize with either part of the queue separately,
   -- which should reduce lock contention as the pending counter and the ready queue are rarely accessed simultaneously.
   --
   -- Task queue users should make sure that all pending and ready tasks have completed before finalizing a queue,
   -- otherwise disaster will ensue. The task queue implementation will attempt to detect this error and report it.
   -- Unfortunately, due to Ada's policy on finalization exceptions, this means Program_Error will be raised.
   --
   type Task_Queue is new Ada.Finalization.Limited_Controlled with
      record
         Ready : Ready_Queue_Implementation.Queue;
         Pending : Pending_Counter;
      end record;

   not overriding procedure Flush (What : Task_Queue);

   not overriding function Is_Empty (What : Task_Queue) return Boolean;

   overriding procedure Finalize (What : in out Task_Queue)
     with Pre => What.Is_Empty;

   Queue_Usage_Error : exception;

   -- Because task queues will be referred to from multiple places, we need some kind of copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   package Atomic_Counters renames Utilities.Atomic_Counters;

   type Pending_Counter is tagged limited
      record
         Implementation : Atomic_Counters.Atomic_Counter;
      end record;

end Asynchronous.Executors.Task_Queues;
