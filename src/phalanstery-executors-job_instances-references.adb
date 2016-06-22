with Ada.Tags;
with Phalanstery.Jobs;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.Job_Instances.References is

   -- DEBUG : I would like this to return a Valid_Reference, but it leads to finalization issues for unclear reasons
   function Make_Job_Instance (From : Interfaces.Any_Async_Job) return Reference is
      Result : constant Reference := Implementation.Make;
   begin
      Result.Set.Job_Object := new Interfaces.Any_Async_Job'(From);
      return Result;
   end Make_Job_Instance;


   -- The remainder of this package is dedicated to unit tests
   type State_Holding_Job is new Jobs.Async_Job with
      record
         Dummy_Int : Natural;
      end record;

   overriding function Run (T        : in out State_Holding_Job;
                            Canceled : Boolean) return Jobs.Return_Value is (Jobs.Return_Finished);

   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Tags.Tag;

      T : constant State_Holding_Job := (Dummy_Int => 42);
      T_Any : constant Interfaces.Any_Async_Job := T;
      T_Instance : constant Valid_Reference := Make_Job_Instance (T);

   begin

      Assert_Truth (Check   => (not T_Instance.Is_Null),
                    Message => "Make_Job_Instance should return an initialized job instance ref");
      Assert_Truth (Check   => (T_Instance.Get.Job_Object /= null),
                    Message => "Make_Job_Instance should allocate job storage as appropriate");
      Assert_Truth (Check   => (T_Instance.Get.Job_Object'Tag = T_Any'Tag),
                    Message => "Make_Job_Instance should allocate job objects of the right type");
      Assert_Truth (Check   => (State_Holding_Job (T_Instance.Get.Job_Object.all).Dummy_Int = 42),
                    Message => "Make_Job_Instance should copy instance data as appropriate");
      Assert_Truth (Check   => (not T_Instance.Get.Completion_Event.Is_Null),
                    Message => "Make_Job_Instance should allocate a completion event");

      T_Instance.Set.Finalize;
      Assert_Truth (Check   => (T_Instance.Get.Job_Object = null),
                    Message => "Job instances should not leak job object storage on finalization");

   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.Job_Instances.References;
