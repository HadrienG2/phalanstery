with Phalanstery.Utilities.References.Nullable;

package Phalanstery.Executors.Job_Queues.References is

   -- Because job queues must be at global scope in order to allow for asynchronous queueing, and must be
   -- shared across multiple tasks, reference counting must be used to manipulate job queues.

   package Implementation_Base is new Utilities.References (Job_Queue);
   package Implementation is new Implementation_Base.Nullable;

   subtype Reference is Implementation.Reference;
   subtype Valid_Reference is Reference with Dynamic_Predicate => (not Valid_Reference.Is_Null);
   function Make_Job_Queue return Valid_Reference renames Implementation.Make;

end Phalanstery.Executors.Job_Queues.References;
