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

with Phalanstery.Outcome_Composition.Interfaces;
with Phalanstery.Outcomes.Interfaces;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.References.Not_Null;
pragma Elaborate_All (Phalanstery.Utilities.References.Not_Null);

package Phalanstery.Outcome_Composition.And_Gates is

   -- This is an implementation of AND gates, a form of outcome object composition which is defined as follows:
   --    - An AND gate with no children is Done
   --    - If any child is Pending, the AND gate is Pending
   --    - If all children are Done, the AND gate is Done
   --    - If any child is Canceled, the AND gate is Canceled
   --    - If any child is in the Error state , the AND gate in the Error state with exception Child_Error
   type And_Gate is new Interfaces.Composite_Outcome with private;

   -- AND gates, like any other form of outcome object composition, are created by grouping outcome objects together
   not overriding procedure Add_Child (Where : in out And_Gate;
                                       Who   : in out Interfaces.Valid_Outcome_Client)
     with Pre => (not Is_Frozen (Where));

   -- Children may be added in a bulk fashion for increased efficiency
   not overriding procedure Add_Children (Where : in out And_Gate;
                                          Who   : in out Interfaces.Valid_Outcome_List)
     with Pre => (not Is_Frozen (Where));

   -- Once all children have been added, one can produce an outcome object associated with the AND gate's outcome.
   not overriding function Make_Client (From : in out And_Gate) return Interfaces.Valid_Outcome_Client
     with Post => (Is_Frozen (From));

   -- After this is done, the AND gate is said to be frozen, which means that it is a run-time error to attempt to
   -- add more children to it. This function may be used to test this status.
   overriding function Is_Frozen (What : And_Gate) return Boolean;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   -- Under the hood, AND gates are implemented using the following protected object
   protected type And_Gate_Implementation is
      procedure Notify_Child_Outcome (What : Outcomes.Interfaces.Final_Outcome_Status);
      procedure Add_Children (Count : Natural);
      procedure Make_Client (Where : out Interfaces.Valid_Outcome_Client);
      function Is_Frozen return Boolean;
   private
      Frozen : Boolean := False;
      Child_Count : Natural := 0;
      Done_Children : Natural := 0;
      Current_Status : Outcomes.Interfaces.Outcome_Status := Outcomes.Interfaces.Pending;
      Outcome : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
      procedure Propagate_Outcome;
   end And_Gate_Implementation;

   -- Like any entity that is shared between multiple tasks, AND gates are best managed using some form of automatic
   -- memory management, such as reference counting.
   package And_Gate_References_Base is new Utilities.References (And_Gate_Implementation);
   package And_Gate_References is new And_Gate_References_Base.Not_Null;
   subtype And_Gate_Reference is And_Gate_References.Reference;

   -- Once we have references to AND gates, we can use them to listen to children outcome objects
   type And_Gate_Child_Listener is new Outcomes.Interfaces.Outcome_Listener_Reference with
      record
         Ref : And_Gate_Reference;
      end record;

   -- This means, of course, implementing the required Notify_Outcome method
   overriding procedure Notify_Outcome (Where : in out And_Gate_Child_Listener;
                                        What  : Outcomes.Interfaces.Final_Outcome_Status);

   -- We can also use AND gate references to build the end user interface to the actual AND gate implementation
   type And_Gate is new Interfaces.Composite_Outcome with
      record
         Ref : And_Gate_Reference;
      end record;

end Phalanstery.Outcome_Composition.And_Gates;
