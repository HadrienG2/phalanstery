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

package Phalanstery.Outcomes.Composition with Pure is

   -- Outcome objects should be composable, in the sense that one should be able to wait for multiple asynchronous
   -- operations to complete. Multiple forms of composition may be envisioned, which can to some degree be related to
   -- to first-order boolean logic gates:
   --    - Wait for all operations to complete, or for either of them to fail ("AND gate", also known as "wait_all")
   --    - Wait for any operation to complete or to fail ("OR gate", also known as "wait_any" or "select")
   --    - Wait for any operation to complete or to fail, then cancel all others ("XOR gate")
   --
   -- At the moment, only AND gate composition is implemented. The implementation of other forms of composition will
   -- need to wait until support for multiple asynchronous operation outcomes is added to Phalanstery.
   --
   -- The children of this package are organized as follows:
   --    - Composition.Interfaces presents some concepts common to all forms of outcome object composition.
   --    - Composition.And_Gates provides an implementation of AND gate composition.
   --    - Composition.Shortcuts provides convenience shortcuts for all forms of outcome composition.

   -- ===================== TODO : The following should be moved to "And_Gates" =======================

   -- The rules for AND-gate composition are the following :
   --    - An AND gate with zero children is Done
   --    - If any child is Pending, the AND gate is Pending
   --    - If all children are Done, the AND gate is Done
   --    - If any child is Canceled, the AND gate is Canceled
   --    - If any child is Error, the AND gate is Error with the special exception Child_Error

   -- =================================================================================================

end Phalanstery.Outcomes.Composition;