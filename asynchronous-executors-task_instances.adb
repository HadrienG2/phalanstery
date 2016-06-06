with Ada.Unchecked_Deallocation;

package body Asynchronous.Executors.Task_Instances is

   overriding procedure Finalize (Who : in out Task_Instance) is
      procedure Liberate_Task is new Ada.Unchecked_Deallocation (Interfaces.Any_Async_Task, Task_Access);
   begin
      Liberate_Task (Who.Task_Object);
   end Finalize;

end Asynchronous.Executors.Task_Instances;
