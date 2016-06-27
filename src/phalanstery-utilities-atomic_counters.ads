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

-- In order to implement reference counting, we need access to some kind of atomic counter.
-- For now, we will use those provided by GNAT. If wider hardware or compiler portability is desired, we
-- will need to find another package, or make our own.
with System.Atomic_Counters;
package Phalanstery.Utilities.Atomic_Counters renames System.Atomic_Counters;
