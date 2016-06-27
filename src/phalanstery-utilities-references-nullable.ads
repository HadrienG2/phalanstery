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

generic
package Phalanstery.Utilities.References.Nullable is

   -- Instances are manipulated through copyable and storable references
   -- In the Nullable case, references are null at creation time and must be explicitly initialized by the user.
   type Reference is new Reference_Base with private;

   -- One can check whether two references point to the same object
   overriding function "=" (A, B : Reference) return Boolean;

   -- Access to the underlying object must be requested explicitly
   overriding function Get (R : Reference) return Accessor
     with Pre => (not R.Is_Null);
   overriding function Set (R : Reference) return Mutator
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

end Phalanstery.Utilities.References.Nullable;
