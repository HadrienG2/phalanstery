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
with Phalanstery.Events.Contracts;
with Phalanstery.Events.Servers;
with Phalanstery.Executors.Interfaces;

package Phalanstery.Executors.Job_Instances is

   -- An asynchronous executor manipulates jobs instances, which are composed of a mutable copy of the source job and
   -- some associated scheduler metadata such as the job's output event.

   -- Job copies must be heap-allocated because we use a class-wide type for them.
   type Job_Access is access Interfaces.Any_Async_Job;

   -- Job instances are currently composed of a job copy and an event used for signaling job completion.
   subtype Valid_Event_Server is Events.Contracts.Valid_Event_Server;
   type Job_Instance is new Ada.Finalization.Limited_Controlled with
      record
         Job_Object : Job_Access := null;
         Completion_Event : Valid_Event_Server := Events.Servers.Make_Event;
      end record;

   -- Because instances must contain pointers, we should make sure that they are always finalized properly
   overriding procedure Finalize (Who : in out Job_Instance);

   -- Because job instances will be moved around, we need some kind of efficiently copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.

end Phalanstery.Executors.Job_Instances;
