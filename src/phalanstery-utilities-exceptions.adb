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

package body Phalanstery.Utilities.Exceptions is

   use type Ada.Exceptions.Exception_Id;

   procedure Make_Occurrence (What  : Ada.Exceptions.Exception_Id;
                              Where : out Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Exceptions.Raise_Exception (What);
   exception
      when E : others =>
         Ada.Exceptions.Save_Occurrence (Target => Where,
                                         Source => E);
   end Make_Occurrence;

   function Is_Occurrence_Of (Who  : Ada.Exceptions.Exception_Occurrence;
                              What : Ada.Exceptions.Exception_Id) return Boolean is
     (Ada.Exceptions.Exception_Identity (Who) = What);

   function Is_Null_Occurrence (Who : Ada.Exceptions.Exception_Occurrence) return Boolean is
     (Is_Occurrence_Of (Who, Ada.Exceptions.Null_Id));

end Phalanstery.Utilities.Exceptions;
