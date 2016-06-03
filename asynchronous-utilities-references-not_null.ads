generic
package Asynchronous.Utilities.References.Not_Null is

   -- Instances are manipulated through copyable and storable references
   -- In the Not_Null variant, these references are initialized with some target object at creation time.
   type Reference is new Reference_Base with private;

   -- One can check whether two references point to the same object
   overriding function "=" (A, B : Reference) return Boolean;

   -- Access to the underlying object must be requested explicitly
   overriding function Get (R : Reference) return Accessor;
   overriding function Set (R : Reference) return Mutator;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Reference is new Reference_Base with
      record
         Instance : Instance_Access;
      end record
   with Type_Invariant => (Reference.Instance /= null);

   overriding procedure Initialize (Who : in out Reference);
   overriding procedure Adjust (Who : in out Reference);
   overriding procedure Finalize (Who : in out Reference);

end Asynchronous.Utilities.References.Not_Null;
