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

with Phalanstery.Events.Composition;
with Phalanstery.Events.Contracts;

package Phalanstery.Jobs is

   -- An asynchronous job is defined as a user-specified, event-driven cooperative multitasking work item.

   -- When I say that asynchronous jobs are event-driven, I mean that they may wait for events to occur, and in general
   -- do so from the start. Thus, we need a data structures representing a list of events for jobs to wait on.
   --
   subtype Valid_Event_Client is Events.Contracts.Valid_Event_Client;
   subtype Event_Wait_List is Events.Composition.Valid_Event_List;

   -- On every run, a job returns status information to the underlying job scheduler.
   -- If a job cannot complete normally, it should raise an exception instead, and the runtime will handle it.
   type Return_Status is (Finished, Yielding, Waiting, Canceled);
   type Return_Value (<>) is private;

   -- Job return values are created in the following way...
   Return_Finished : constant Return_Value;
   Return_Yielding : constant Return_Value;
   Return_Canceled : constant Return_Value;
   function Return_Waiting (Cause : Valid_Event_Client) return Return_Value;
   function Return_Waiting (Cause : Event_Wait_List) return Return_Value;

   -- ...and queried in the following way
   function Status (What : Return_Value) return Return_Status;
   function Wait_List (What : Return_Value) return Event_Wait_List
     with Pre => (Status (What) = Waiting);

   -- Asynchronous jobs are user-defined, by inheriting from a common interface.
   --
   -- One thing to keep in mind is that for job queueing and load balancing to be efficient, you should design job
   -- types to be cheap to copy, for example by having them host pointers or references to arrays instead of raw arrays.
   --
   -- Another important consideration is that in order to follow Ada's accessibility rules, job types should currently
   -- be defined at global scope. The reason is that asynchronous job objects might otherwise outlive their type.
   --
   -- Job cancelation is handled as follows: dependents of the canceled job are canceled immediately, and the job
   -- itself gets notified about the cancelation through a parameter to Run. A job may also choose to cancel itself,
   -- along with its dependents, by returning Return_Canceled.
   --
   type Async_Job is interface;
   function Run (Who          : in out Async_Job;
                 Was_Canceled : Boolean) return Return_Value is abstract;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Return_Value (State : Return_Status; Wait_List_Length : Natural) is
      record
         case State is
            when Finished | Yielding | Canceled =>
               null;
            when Waiting =>
               Wait_List : Event_Wait_List (1 .. Wait_List_Length);
         end case;
      end record;

   Return_Finished : constant Return_Value := (State => Finished, Wait_List_Length => 0);
   Return_Yielding : constant Return_Value := (State => Yielding, Wait_List_Length => 0);
   Return_Canceled : constant Return_Value := (State => Canceled, Wait_List_Length => 0);

end Phalanstery.Jobs;
