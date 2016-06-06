package body Asynchronous.Executors.Task_Instances.References is

   function Make_Task_Instance (From : Interfaces.Any_Async_Task) return Task_Instance_Reference is
   begin
      return Ref : constant Task_Instance_Reference := Task_Instance_References.Make do
         Ref.Set.Task_Object := new Interfaces.Any_Async_Task'(From);
      end return;
   end Make_Task_Instance;

end Asynchronous.Executors.Task_Instances.References;
