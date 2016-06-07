with Ada.Finalization;
with Asynchronous.Utilities.Atomic_Counters;

generic
   type Object is limited private;
package Asynchronous.Utilities.References is

   -- This package familly manages reference-counted object instances
   type Mutator is not null access all Object;
   type Accessor is not null access constant Object;

   -- Instances are manipulated through copyable and storable references.
   -- Each reference type should define its own controlled semantics depending on the constraints it operates under.
   type Reference_Base is abstract new Ada.Finalization.Controlled with private;

   -- One can check whether two references point to the same object
   overriding function "=" (A, B : Reference_Base) return Boolean is abstract;

   -- Access to the underlying object must be requested explicitly
   function Get (R : Reference_Base) return Accessor is abstract;
   function Set (R : Reference_Base) return Mutator is abstract;

private

   type Packaged_Instance is limited
      record
         Data : aliased Object;
         Reference_Count : Atomic_Counters.Atomic_Counter;
      end record;

   type Instance_Access is access Packaged_Instance;

   type Reference_Base is abstract new Ada.Finalization.Controlled with null record;

end Asynchronous.Utilities.References;
