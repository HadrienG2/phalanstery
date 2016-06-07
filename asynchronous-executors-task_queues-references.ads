with Asynchronous.Utilities.References.Nullable;

package Asynchronous.Executors.Task_Queues.References is

   -- Because task queues must be at global scope in order to allow for asynchronous queueing, and must be
   -- shared across multiple tasks, reference counting must be used to manipulate task queues.

   package Implementation_Base is new Utilities.References (Task_Queue);
   package Implementation is new Implementation_Base.Nullable;

   subtype Reference is Implementation.Reference;
   function Make_Task_Queue return Reference renames Implementation.Make;

end Asynchronous.Executors.Task_Queues.References;
