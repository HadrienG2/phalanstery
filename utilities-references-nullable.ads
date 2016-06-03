generic
package Utilities.References.Nullable is

   -- Instances are manipulated through copyable and storable references
   -- In the Nullable case, references are null at creation time and must be explicitly initialized by the user.
   type Reference is new Reference_Base with private;

   -- One can check whether two references point to the same object
   overriding function "=" (A, B : Reference) return Boolean;

   -- Access to the underlying object must be requested explicitly
   overriding function Get (R : Reference) return Accessor
     with Pre => (not R.Is_Null);

   -- In the case of nullable references, we have one more caveat to take care of: the null state
   not overriding function Is_Null (R : Reference) return Boolean;

   -- And explicit initialization is another nasty thing to take care of
   not overriding function Make return Reference
     with Post => (not Make'Result.Is_Null);

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Reference is new Reference_Base with
      record
         Instance : Instance_Access := null;
      end record;

   overriding procedure Adjust (Who : in out Reference);
   overriding procedure Finalize (Who : in out Reference);

end Utilities.References.Nullable;
