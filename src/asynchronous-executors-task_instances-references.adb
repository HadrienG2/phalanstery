with Ada.Tags;
with Asynchronous.Tasks;
with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Executors.Task_Instances.References is

   -- DEBUG : I would like this to return a Valid_Reference, but it leads to finalization issues for unclear reasons
   function Make_Task_Instance (From : Interfaces.Any_Async_Task) return Reference is
      Result : constant Reference := Implementation.Make;
   begin
      Result.Set.Task_Object := new Interfaces.Any_Async_Task'(From);
      return Result;
   end Make_Task_Instance;


   -- The remainder of this package is dedicated to unit tests
   type State_Holding_Task is new Tasks.Async_Task with
      record
         Dummy_Int : Natural;
      end record;

   overriding function Run (T        : in out State_Holding_Task;
                            Canceled : Boolean) return Tasks.Return_Value is (Tasks.Return_Finished);

   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Tags.Tag;

      T : constant State_Holding_Task := (Dummy_Int => 42);
      T_Any : constant Interfaces.Any_Async_Task := T;
      T_Instance : constant Valid_Reference := Make_Task_Instance (T);

   begin

      Assert_Truth (Check   => (not T_Instance.Is_Null),
                    Message => "Make_Task_Instance should return an initialized task instance ref");
      Assert_Truth (Check   => (T_Instance.Get.Task_Object /= null),
                    Message => "Make_Task_Instance should allocate task storage as appropriate");
      Assert_Truth (Check   => (T_Instance.Get.Task_Object'Tag = T_Any'Tag),
                    Message => "Make_Task_Instance should allocate task objects of the right type");
      Assert_Truth (Check   => (State_Holding_Task (T_Instance.Get.Task_Object.all).Dummy_Int = 42),
                    Message => "Make_Task_Instance should copy instance data as appropriate");
      Assert_Truth (Check   => (not T_Instance.Get.Completion_Event.Is_Null),
                    Message => "Make_Task_Instance should allocate a completion event");

      T_Instance.Set.Finalize;
      Assert_Truth (Check   => (T_Instance.Get.Task_Object = null),
                    Message => "Task instances should not leak task object storage on finalization");

   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Asynchronous.Executors.Task_Instances.References;
