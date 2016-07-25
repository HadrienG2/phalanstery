-- Copyright 2016 Hadrien Grasland
--
-- This file is part of Phalanstery.
--
-- Phalanstery is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Phalanstery is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Phalanstery.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Tags;
with Phalanstery.Asynchronous_Jobs;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.Job_Instances.References is

   function Make_Job_Instance (From : Interfaces.Any_Asynchronous_Job) return Valid_Reference is
      Result : constant Reference := Implementation.Make;
   begin
      Result.Set.Job_Object := new Interfaces.Any_Asynchronous_Job'(From);
      return Valid_Reference (Result);
   end Make_Job_Instance;


   -- The remainder of this package is dedicated to unit tests
   type Stateful_Job is new Interfaces.Asynchronous_Job with
      record
         Dummy_Int : Natural;
      end record;

   overriding function Run (T        : in out Stateful_Job;
                            Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
     (Asynchronous_Jobs.Return_Finished);

   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Tags.Tag;

      T : constant Stateful_Job := (Interfaces.Asynchronous_Job with Dummy_Int => 42);
      T_Any : constant Interfaces.Any_Asynchronous_Job := T;
      T_Instance : constant Valid_Reference := Make_Job_Instance (T);

   begin

      Assert_Truth (Check   => (not T_Instance.Is_Null),
                    Message => "Make_Job_Instance should return an initialized job instance ref");
      Assert_Truth (Check   => (T_Instance.Get.Job_Object /= null),
                    Message => "Make_Job_Instance should allocate job storage as appropriate");
      Assert_Truth (Check   => (T_Instance.Get.Job_Object'Tag = T_Any'Tag),
                    Message => "Make_Job_Instance should allocate job objects of the right type");
      Assert_Truth (Check   => (Stateful_Job (T_Instance.Get.Job_Object.all).Dummy_Int = 42),
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
