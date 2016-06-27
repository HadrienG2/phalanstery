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

with Phalanstery.Events.Clients;
with Phalanstery.Events.Servers;

package Phalanstery.Events.Contracts is

   -- Although allowing events handles to be null is great for performance, it is not generally desirable.
   -- We thus propose subtypes of event handles which are guaranteed by contract programming not to be null.

   subtype Valid_Event_Client is Events.Clients.Client
     with Dynamic_Predicate => (not Valid_Event_Client.Is_Null);

   subtype Valid_Event_Server is Events.Servers.Server
     with Dynamic_Predicate => (not Valid_Event_Server.Is_Null);

end Phalanstery.Events.Contracts;
