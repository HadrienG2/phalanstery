with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Asynchronous.Executors.Task_Instances.References;
with Asynchronous.Utilities.References.Nullable;

private package Asynchronous.Executors.Task_Queues is

   -- Ready asynchronous tasks will be put on a FIFO queue, and executed once worker threads become available.
   subtype Task_Instance_Reference is Task_Instances.References.Reference;
   package Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces (Task_Instance_Reference);
   package Queue_Implementation is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces);

   -- Because task queues must be at global scope in order to allow for asynchronous queueing, and must be
   -- shared across multiple tasks, reference counting must be used here as well.
   package References_Base is new Utilities.References (Queue_Implementation.Queue);
   package References is new References_Base.Nullable;
   subtype Reference is References.Reference;
   function Make_Task_Queue return Reference renames References.Make;

end Asynchronous.Executors.Task_Queues;
