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
with Phalanstery.Outcomes.Servers;

package Phalanstery.Outcomes.Contracts is

   -- Because creating an outcome object involves dynamic memory allocation, which is a relatively expensive operation
   -- from a performance point of view, we do not want to do it unnecessarily.
   --
   -- This is why we allow outcome client and servers to be initially in an invalid null state, and only subsequently
   -- initialized with a valid reference to an outcome object as that reference becomes available.
   --
   -- However, in most cases, we want to manipulate valid outcome clients and servers. We express this intent using
   -- a subtype with a suitable predicates.

   subtype Valid_Outcome_Client is Outcomes.Clients.Client
     with Dynamic_Predicate => (not Valid_Outcome_Client.Is_Null);

   subtype Valid_Outcome_Server is Outcomes.Servers.Server
     with Dynamic_Predicate => (not Valid_Outcome_Server.Is_Null);

end Phalanstery.Outcomes.Contracts;
