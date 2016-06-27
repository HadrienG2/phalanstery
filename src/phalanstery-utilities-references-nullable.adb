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

with Ada.Assertions;
with Ada.Unchecked_Deallocation;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Utilities.References.Nullable is

   overriding function "=" (A, B : Reference) return Boolean is (A.Instance = B.Instance);

   overriding function Get (R : Reference) return Accessor is (R.Instance.Data'Access);

   overriding function Set (R : Reference) return Mutator is (R.Instance.Data'Access);

   not overriding function Is_Null (R : Reference) return Boolean is (R.Instance = null);

   not overriding function Make return Reference is
     (Reference'(Reference_Base with Instance => new Packaged_Instance));

   overriding procedure Adjust (Who : in out Reference) is
   begin
      if not Who.Is_Null then
         Atomic_Counters.Increment (Who.Instance.Reference_Count);
      end if;
   end Adjust;

   overriding procedure Finalize (Who : in out Reference) is
      procedure Free_Instance is new Ada.Unchecked_Deallocation (Packaged_Instance, Instance_Access);
   begin
      if not Who.Is_Null and then Atomic_Counters.Decrement (Who.Instance.Reference_Count) then
         Free_Instance (Who.Instance);
      else
         Who.Instance := null;  -- Handle multiple finalization (as allowed by the Ada standard)
      end if;
   end Finalize;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Initial_Value is
         R : Reference;
      begin
         Assert_Truth (Check   => (R.Is_Null) and (R.Instance = null),
                       Message => "Newly created references should be null in every sense of the word");
      end Test_Initial_Value;

      procedure Test_Make is
         R : constant Reference := Make;
      begin
         Assert_Truth (Check   => (not R.Is_Null) and (R.Instance /= null),
                       Message => "A reference coming from Make should be non-null");
         Assert_Truth (Check   => Atomic_Counters.Is_One (R.Instance.Reference_Count),
                       Message => "Freshly created references should have a reference count of one");
      end Test_Make;

      procedure Test_Adjust is
         R1 : Reference;
         R2 : constant Reference := Make;
      begin
         Assert_Truth (Check   => (R1.Instance /= R2.Instance),
                       Message => "A null and non-null reference should not be equal");

         R1 := Make;
         Assert_Truth (Check   => (R1.Instance /= R2.Instance),
                       Message => "Two separately created non-null reference should not be equal");

         R1 := R2;
         Assert_Truth (Check   => (R1.Instance = R2.Instance),
                       Message => "After assignment, two references should be equal");
      end Test_Adjust;

      procedure Test_Equality is
         R1, R2 : Reference;
      begin
         Assert_Truth (Check   => (R1 = R2),
                       Message => "Two newly created nullable references should be equal, as they are null");

         R1 := Make;
         Assert_Truth (Check   => (R1 /= R2),
                       Message => "A null and non-null reference should not be equal");

         R2 := Make;
         Assert_Truth (Check   => (R1 /= R2),
                       Message => "Two separately created non-null reference should not be equal");

         R1 := R2;
         Assert_Truth (Check   => (R1 = R2),
                       Message => "After assignment, two references should be equal");
      end Test_Equality;

      procedure Test_Get is
         R : Reference;
      begin
         begin
            declare
               Unused : constant Accessor := R.Get with Unreferenced;
            begin
               Fail ("Accessing null references should fail with Constraint_Error or Assertion_Error");
            end;
         exception
            when Constraint_Error | Ada.Assertions.Assertion_Error =>
               null;
         end;

         R := Make;
         Assert_Truth (Check   => (R.Get = R.Instance.Data'Access),
                       Message => "Get method of references should provide access to the underlying data object");
      end Test_Get;

      procedure Test_Finalize is
         R1 : constant Reference := Make;
         R2 : Reference := R1;
      begin
         Finalize (R2);
         Assert_Truth (Check   => Atomic_Counters.Is_One (R1.Instance.Reference_Count),
                       Message => "Reference counts should go back to one after copy finalization");

         Finalize (R2);
         Assert_Truth (Check   => Atomic_Counters.Is_One (R1.Instance.Reference_Count),
                       Message => "Reference counts should be unaffected by multiple finalization");

         begin
            declare
               R3 : Reference with Unreferenced;
            begin
               null;
            end;
         exception
            when others =>
               Fail ("Finalizing a null reference should not fail");
         end;
      end Test_Finalize;

   begin
      Test_Initial_Value;
      Test_Make;
      Test_Adjust;
      Test_Equality;
      Test_Get;
      Test_Finalize;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Utilities.References.Nullable;
