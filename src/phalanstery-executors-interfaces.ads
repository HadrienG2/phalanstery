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

with Phalanstery.Asynchronous_Jobs;
with Phalanstery.Outcomes.Contracts;
with Phalanstery.Outcome_Composition.Interfaces;
with System.Multiprocessors;

package Phalanstery.Executors.Interfaces is

   -- This package holds declarations which are likely to be shared by any executor implementation

   subtype Asynchronous_Job is Asynchronous_Jobs.Asynchronous_Job;
   subtype Any_Asynchronous_Job is Asynchronous_Job'Class;

   subtype Valid_Outcome_Client is Outcomes.Contracts.Valid_Outcome_Client;
   subtype Valid_Outcome_Server is Outcomes.Contracts.Valid_Outcome_Server;

end Phalanstery.Executors.Interfaces;
