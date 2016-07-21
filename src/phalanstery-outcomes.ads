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

package Phalanstery.Outcomes with Pure is

   -- Being able to start asynchronous jobs is of limited use unless there's a way to synchronize with them. We thus
   -- want some object that will tell us when a job will terminate, and in which circumstances it terminated (did it
   -- complete as expected, or fail with an error, for example?). In addition to propagating error, being able to
   -- request the cancelation of a job is another very useful feature.
   --
   -- In Phalanstery, this functionality is grouped into so-called outcome objects, which are spawned anytime a job
   -- is scheduled for execution, and are also intended to be applicable as an interface to more than Phalanstery jobs,
   -- also encompassing other asynchronous facilities such as OS-provided asynchronous IO when available.
   --
   -- The outcome object implementation is decomposed into child packages as follows:
   --    - Outcomes.Interfaces defines the basic client/server interface followed by outcome objects
   --    - Outcomes.Implementation implements outcome synchronization using a reference-counted protected object.
   --    - Outcomes.Clients and Outcomes.Servers complete the implementation of the outcome interface.
   --
   -- In addition, the following outcome-related convenience packages are provided:
   --    - Outcomes.Callbacks provides the simplest example of an outcome listener : a global callback procedure.
   --    - Outcomes.Contracts provide some specialized contracts on top of the general outcome interface.

end Phalanstery.Outcomes;
