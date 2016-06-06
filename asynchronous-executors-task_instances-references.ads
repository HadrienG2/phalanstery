with Asynchronous.Utilities.References.Nullable;

-- TODO : Should be made private
package Asynchronous.Executors.Task_Instances.References is

   -- To be able to move task instances around, we need some kind of reference to them. In C++ terms, what we would
   -- like most is a unique_ptr, as we only need move semantics and not copy semantics. But a shared reference, although
   -- somewhat inefficient from a performance point of view, will do fine as a first implementation.
   package Implementation_Base is new Utilities.References (Task_Instance);
   package Implementation is new Implementation_Base.Nullable;
   subtype Reference is Implementation.Reference;

   -- There should be a convenient way to make a reference-counted task instance from a task object
   function Make_Task_Instance (From : Interfaces.Any_Async_Task) return Reference;

end Asynchronous.Executors.Task_Instances.References;
