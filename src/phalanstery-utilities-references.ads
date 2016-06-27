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

with Ada.Finalization;
with Phalanstery.Utilities.Atomic_Counters;

generic
   type Object is limited private;
package Phalanstery.Utilities.References is

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

end Phalanstery.Utilities.References;
