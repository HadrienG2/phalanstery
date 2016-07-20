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

package Phalanstery.Outcomes.Composition.Interfaces is

   -- This type expresses a list of asynchronous operations that one wants to simultaneously synchronize with
   type Wait_List is array (Positive range <>) of Outcomes.Clients.Client;

   -- In this package, we only want to deal with valid outcome objects, or lists thereof
   subtype Valid_Outcome_Client is Contracts.Valid_Outcome_Client;

   subtype Valid_Outcome_Server is Contracts.Valid_Outcome_Server;

   subtype Valid_Wait_List is Wait_List
     with Dynamic_Predicate => (for all E of Valid_Wait_List => E in Valid_Outcome_Client);

   -- In general, failure of a composite outcome object means failure of at least one of the outcome objects that it is
   -- made of, and possibly several of them at once. Full-blown error reporting is difficult to implement in this
   -- situation, and may not be truly needed. For now, we opt to simply use a generic Ada exception to report this.
   Child_Error : exception;

end Phalanstery.Outcomes.Composition.Interfaces;
