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

with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Contracts;

package Phalanstery.Outcome_Composition.Interfaces is

   -- === PRELIMINARY DEFINITIONS ===

   -- This type expresses a list of asynchronous operations that one wants to synchronize with concurrently
   type Outcome_List is array (Positive range <>) of Outcomes.Clients.Client;

   -- In this package, we only want to deal with valid outcome objects, or lists thereof
   subtype Valid_Outcome_Client is Outcomes.Contracts.Valid_Outcome_Client;
   subtype Valid_Outcome_Server is Outcomes.Contracts.Valid_Outcome_Server;
   subtype Valid_Outcome_List is Outcome_List
     with Dynamic_Predicate => (for all E of Valid_Outcome_List => E in Valid_Outcome_Client);


   -- === COMPOSITE OUTCOME INTERFACE ===

   -- This is the interface common to all composite outcome objects. Not a lot of functionality has been moved to this
   -- interface yet, as we do not know yet how much of it is specific to AND gates.
   type Composite_Outcome is interface;

   -- Composite outcome objects are built by combining multiple outcome objects. At some point, one can extract an
   -- outcome object from them, which represents the completion of the requested combination of operations. After this
   -- is done, the composite outcome object is said to be frozen, and it is an error to attempt to add more children.
   function Is_Frozen (What : Composite_Outcome) return Boolean is abstract;

   -- If one attempts to do so nevertheless, the following run-time exception will be raised.
   Composite_Outcome_Already_Frozen : exception;

   -- Error handling in composite outcome objects requires some care. In general, failure of the composite outcome can
   -- originate from the failure of any of the outcome objects that it is made of, and possibly a combination thereof.
   -- Because reporting these errors accurately is diffcult, and may not be actually needed, we opt to use a generic
   -- catch-all Ada exception in this situation. This exception will be propagated to the final outcome object.
   Child_Error : exception;

end Phalanstery.Outcome_Composition.Interfaces;
