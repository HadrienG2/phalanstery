with Ada.Unchecked_Deallocation;

package body Phalanstery.Executors.Job_Instances is

   overriding procedure Finalize (Who : in out Job_Instance) is
      procedure Liberate_Job is new Ada.Unchecked_Deallocation (Interfaces.Any_Async_Job, Job_Access);
   begin
      Liberate_Job (Who.Job_Object);
   end Finalize;

end Phalanstery.Executors.Job_Instances;
