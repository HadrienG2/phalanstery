package body Asynchronous.Executors.Task_Instances.References is

   function Make_Task_Instance (From : Interfaces.Any_Async_Task) return Reference is
   begin
      return Ref : constant Reference := Implementation.Make do
         Ref.Set.Task_Object := new Interfaces.Any_Async_Task'(From);
      end return;
   end Make_Task_Instance;

end Asynchronous.Executors.Task_Instances.References;
