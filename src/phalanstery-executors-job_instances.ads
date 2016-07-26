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
with Phalanstery.Outcomes.Servers;
with Phalanstery.Executors.Interfaces;

package Phalanstery.Executors.Job_Instances is

   -- A Phalanstery executor manipulates jobs instances, which are composed of a mutable copy of the source job and
   -- some unique scheduler metadata such as the outcome object associated with that job instance.

   -- Job copies must be heap-allocated because we use a class-wide type for them.
   type Job_Access is access Interfaces.Any_Asynchronous_Job;

   -- Currently, a job instance is composed of a copy of the job and the outcome object associated with it
   type Job_Instance is new Ada.Finalization.Limited_Controlled with
      record
         Job_Object : Job_Access := null;
         Outcome : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
      end record;

   -- Because job instances contain pointers, we should make sure that they are always finalized properly
   overriding procedure Finalize (Who : in out Job_Instance);

   -- Because job instances will be moved around, we need some kind of efficiently copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.
   -- This is also where unit tests will be located, since we'll be working with these references most of the time.

end Phalanstery.Executors.Job_Instances;
