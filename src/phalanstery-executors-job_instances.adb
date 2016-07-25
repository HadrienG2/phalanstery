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

with Ada.Unchecked_Deallocation;

package body Phalanstery.Executors.Job_Instances is

   overriding procedure Finalize (Who : in out Job_Instance) is
      procedure Liberate_Job is new Ada.Unchecked_Deallocation (Interfaces.Any_Asynchronous_Job, Job_Access);
   begin
      Liberate_Job (Who.Job_Object);
   end Finalize;

end Phalanstery.Executors.Job_Instances;
