with Phalanstery.Utilities.References.Nullable;

package Phalanstery.Executors.Job_Instances.References is

   -- To be able to move job instances around, we need some kind of reference to them. In C++ terms, what we would
   -- like most is a unique_ptr, as we only need move semantics and not copy semantics. But a shared reference, although
   -- somewhat inefficient from a performance point of view, will do fine as a first implementation.
   package Implementation_Base is new Utilities.References (Job_Instance);
   package Implementation is new Implementation_Base.Nullable;
   subtype Reference is Implementation.Reference;

   -- Among all possible job instance references, only a subset is suitable for everyday use
   subtype Valid_Reference is Reference with
     Dynamic_Predicate => ((not Valid_Reference.Is_Null) and then not (Valid_Reference.Get.Job_Object = null));

   -- There should be a convenient way to make a reference-counted job instance from a job object
   function Make_Job_Instance (From : Interfaces.Any_Async_Job) return Reference;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Executors.Job_Instances.References;
