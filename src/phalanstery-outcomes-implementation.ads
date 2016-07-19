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

with Ada.Containers.Indefinite_Vectors;
with Ada.Exceptions;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Utilities.References;
with Phalanstery.Utilities.References.Nullable;
pragma Elaborate_All (Phalanstery.Utilities.References.Nullable);

package Phalanstery.Outcomes.Implementation is

   -- This package features an implementation of outcome objects as described by Outcomes.Interfaces.
   -- It does not implement outcome clients and servers directly, for that see Outcomes.Clients and Outcomes.Servers.

   use type Interfaces.Outcome_Listener_Reference;

   -- First, we define a container to store references to the listeners of the outcome object
   package Outcome_Listener_Lists is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => Interfaces.Outcome_Listener_Reference'Class);

   subtype Outcome_Listener_List is Outcome_Listener_Lists.Vector;

   -- Then we can define our outcome objects
   protected type Outcome is

      function Status return Interfaces.Outcome_Status;
      procedure Get_Error (What : out Ada.Exceptions.Exception_Occurrence);

      entry Wait_Completion;

      procedure Add_Listener (Who : in out Interfaces.Outcome_Listener_Reference'Class);

      procedure Cancel;
      function Is_Canceled return Boolean;

      procedure Mark_Done;
      procedure Mark_Error (What : Ada.Exceptions.Exception_Occurrence);

   private

      Current_Status : Interfaces.Outcome_Status := Interfaces.Pending;
      Current_Error : Ada.Exceptions.Exception_Occurrence;
      Listeners : Outcome_Listener_List;

      procedure Notify_Listeners;

   end Outcome;

   -- And finally, we can define references to these outcome objects
   package Outcome_References_Base is new Utilities.References (Outcome);
   package Outcome_References is new Outcome_References_Base.Nullable;
   subtype Outcome_Reference is Outcome_References.Reference;
   function Make_Outcome return Outcome_Reference renames Outcome_References.Make;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Outcomes.Implementation;
