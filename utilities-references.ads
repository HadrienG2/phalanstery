with Ada.Finalization;

generic
   type Object is limited private;
package Utilities.References is

   -- This package familly manages reference-counted object instances
   type Accessor is access all Object;

   -- Instances are manipulated through copyable and storable references.
   -- Each reference type should define its own controlled semantics depending on the constraints it operates under.
   type Reference_Base is abstract new Ada.Finalization.Controlled with private;

   -- One can check whether two references point to the same object
   function "=" (A, B : Reference_Base) return Boolean is abstract;

   -- Access to the underlying object must be requested explicitly
   function Get (R : Reference_Base) return Accessor is abstract;

private

   type Reference_Base is abstract new Ada.Finalization.Controlled with null record;

end Utilities.References;
