with Asynchronous.Utilities.References.Nullable;

-- TODO : Should be made private
package Asynchronous.Executors.Task_Instances.References is

   -- To be able to move task instances around, we need some kind of reference to them. In C++ terms, what we would
   -- like most is a unique_ptr, as we only need move semantics and not copy semantics. But a shared reference, although
   -- somewhat inefficient from a performance point of view, will do fine as a first implementation.
   package Task_Instance_Reference_Base is new Utilities.References (Task_Instance);
   package Task_Instance_References is new Task_Instance_Reference_Base.Nullable;
   subtype Task_Instance_Reference is Task_Instance_References.Reference;

   -- There should be a convenient way to make a reference-counted task instance from a task object
   function Make_Task_Instance (From : Interfaces.Any_Async_Task) return Task_Instance_Reference;

end Asynchronous.Executors.Task_Instances.References;
