with Ada.Unchecked_Deallocation;
with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Utilities.References.Not_Null is

   function "=" (A, B : Reference) return Boolean is (A.Instance = B.Instance);

   function Get (R : Reference) return Accessor is (R.Instance.Data'Access);

   function Set (R : Reference) return Mutator is (R.Instance.Data'Access);

   overriding procedure Initialize (Who : in out Reference) is
   begin
      Who.Instance := new Packaged_Instance;
   end Initialize;

   overriding procedure Adjust (Who : in out Reference) is
   begin
      Atomic_Counters.Increment (Who.Instance.Reference_Count);
   end Adjust;

   overriding procedure Finalize (Who : in out Reference) is
      procedure Free_Instance is new Ada.Unchecked_Deallocation (Packaged_Instance, Instance_Access);
   begin
      -- Normal reference counting workflow
      if Who.Instance /= null and then Atomic_Counters.Decrement (Who.Instance.Reference_Count) then
         Free_Instance (Who.Instance);
      end if;

      -- Multiple finalization management
      Who.Instance := null;
   end Finalize;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;

      procedure Test_Initialize is
         A : Reference;
      begin
         Assert_Truth (Check   => (A.Instance /= null),
                       Message => "Freshly created references should be non-null");
         Assert_Truth (Check   => Atomic_Counters.Is_One (A.Instance.Reference_Count),
                       Message => "Freshly created references should have a reference count of one");
      end Test_Initialize;

      procedure Test_Adjust is
         A1 : Reference;
         A2 : constant Reference := A1;
      begin
         Assert_Truth (Check   => (A1.Instance = A2.Instance),
                       Message => "Assignment should make references equal");
         Assert_Truth (Check   => (A2.Instance /= null),
                       Message => "References should not become null after assignment");
         Assert_Truth (Check   => not Atomic_Counters.Is_One (A1.Instance.Reference_Count),
                       Message => "The reference count should be incremented on Adjust");
      end Test_Adjust;

      procedure Test_Equality is
         A1, A2 : Reference;
      begin
         Assert_Truth (Check   => (A1 /= A2) and (A1.Instance /= A2.Instance),
                       Message => "Two separately created references should not appear equal, nor be so");
         A1 := A2;
         Assert_Truth (Check   => (A1 = A2) and (A1.Instance = A2.Instance),
                       Message => "After assignment, two references should appear equal and actually be");
      end Test_Equality;

      procedure Test_Get is
         A : Reference;
      begin
         Assert_Truth (Check   => (A.Get = A.Instance.Data'Access),
                       Message => "Get method of references should provide access to the underlying data object");
      end Test_Get;

      procedure Test_Finalize is
         A1 : Reference;
      begin
         declare
            A2 : constant Reference := A1 with Unreferenced;
         begin
            null;
         end;
         Assert_Truth (Check   => Atomic_Counters.Is_One (A1.Instance.Reference_Count),
                       Message => "Reference counts should go back to one after copy finalization");
      end Test_Finalize;

   begin
      Test_Initialize;
      Test_Adjust;
      Test_Equality;
      Test_Get;
      Test_Finalize;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Asynchronous.Utilities.References.Not_Null;
