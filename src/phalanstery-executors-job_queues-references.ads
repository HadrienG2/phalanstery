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

with Phalanstery.Utilities.References.Not_Null;

package Phalanstery.Executors.Job_Queues.References is

   -- Because job queues must be at global scope in order to allow for asynchronous queueing, and must be
   -- shared across multiple tasks, reference counting must be used to manipulate job queues.

   package Implementation_Base is new Utilities.References (Job_Queue);
   package Implementation is new Implementation_Base.Not_Null;

   subtype Valid_Reference is Implementation.Reference;

end Phalanstery.Executors.Job_Queues.References;
