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

with Phalanstery.Events.Contracts;
with Phalanstery.Jobs;
with System.Multiprocessors;

package Phalanstery.Executors.Interfaces is

   -- This package holds declarations which are likely to be shared by any executor implementation

   subtype Any_Async_Job is Asynchronous_Jobs.Async_Job'Class;
   subtype Valid_Outcome_Client is Outcomes.Contracts.Valid_Event_Client;
   subtype Valid_Outcome_List is Asynchronous_Jobs.Valid_Outcome_List;

end Phalanstery.Executors.Interfaces;
