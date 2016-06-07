with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Finalization;
with Asynchronous.Executors.Task_Instances.References;

package Asynchronous.Executors.Task_Queues is

   -- Ready asynchronous tasks will be put on a FIFO queue, and executed once worker threads become available.
   subtype Task_Instance_Reference is Task_Instances.References.Reference;
   package Ready_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Task_Instance_Reference);
   package Ready_Queue_Implementation is new Ada.Containers.Unbounded_Synchronized_Queues (Ready_Queue_Interfaces);

   -- Pending tasks do not really follow an organized data structure: a task may become pending at any time, and resume
   -- execution at any other time, regardless of what any other task is doing. Thus, pending tasks will be stored on the
   -- heap (see Executors.Scheduling for more details), and we only track how many there are in a centralized manner.
   protected type Pending_Counter is

      -- Increment or decrement the pending task counter
      procedure Add_Task;
      procedure Remove_Task;

      -- Tell whether all pending tasks have completed
      function No_Pending_Task return Boolean;

      -- Wait until all pending tasks have completed
      entry Flush_Pending;

   private
      Count : Natural := 0;
   end Pending_Counter;

   -- A task queue is a combination of a ready task queue and a pending task counter. Task queue users should make sure
   -- that all pending tasks have completed before finalizing a queue. The task queue implementation will attempt to
   -- detect this error and report it as an exception.
   type Task_Queue is new Ada.Finalization.Limited_Controlled with
      record
         Ready : Ready_Queue_Implementation.Queue;
         Pending : Pending_Counter;
      end record;

   not overriding function Is_Empty (What : Task_Queue) return Boolean;

   overriding procedure Finalize (What : in out Task_Queue)
     with Pre => What.Is_Empty;

   Queue_Usage_Error : exception;

   -- Because task queues will be referred to from multiple places, we need some kind of copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.

end Asynchronous.Executors.Task_Queues;
