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

with Ada.Exceptions;

package Phalanstery.Outcomes.Interfaces with Preelaborate is

   -- === COMMON OUTCOME OBJECT INTERFACE ===

   -- An asynchronous operation may have multiple outcomes. We represent this programmatically by having outcome objects
   -- that hold an internal state machine, which goes from an indefinite "pending" state to one of several final states.
   type Outcome_Status is (Pending, Done, Canceled, Error);
   subtype Final_Outcome_Status is Outcome_Status range Done .. Error;

   -- Beacause outcomes are used to synchronize multiple tasks, they must be stored in some shared memory area and
   -- accessed by reference. These references have semantics similar to those of Ada's general access types.
   type Outcome_Reference is limited interface;
   function Is_Null (Who : Outcome_Reference) return Boolean is abstract;
   function "=" (A, B : Outcome_Reference) return Boolean is abstract;

   -- Through an outcome object, one can request the cancelation of the associated asynchronous operation. The effect of
   -- cancelation depend on how far the operation has progressed:
   --    * If the operation has not started yet, if won't run, and operations depending on it will be canceled as well.
   --    * If the operation is running, it will be notified of the cancelation request, will process it in an
   --      implementation-defined way. Dependents of the operation will still be canceled as before.
   --    * If the operation has already completed, attempting to cancel it will have no effect.
   procedure Cancel (Who : in out Outcome_Reference) is abstract
     with Pre'Class => (not Who.Is_Null);


   -- === CLIENT-SPECIFIC INTERFACE ===

   -- Clients of an outcome object represent actors which are not directly involved in an asynchronous operation, but
   -- are able to track its progress and cancel it. Any code which starts an asynchronous operation will receive a
   -- client to the outcome of that operation.
   type Outcome_Client is interface and Outcome_Reference;

   -- The simplest outcome client interaction is to poll the current state of the outcome object. This query is fast and
   -- nonblocking, however it should not be run in a loop to wait for the completion of an asynchronous operation, as
   -- doing so wastes CPU time and can lead to deadlocks.
   function Status (Who : Outcome_Client) return Outcome_Status is abstract
     with Pre'Class => (not Who.Is_Null);

   -- When an asynchronous operation has failed because of an error, the outcome object associated to this operation
   -- will hold the associated Ada exception occurence and may be queried for it.
   procedure Get_Error (Who  : Outcome_Client;
                        What : out Ada.Exceptions.Exception_Occurrence) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- It is also possible to wait for the completion of an asynchronous operation in a CPU-efficient fashion by using
   -- the OS' blocking wait mechanism. However, since this mechanism blocks the underlying OS thread, it should not be
   -- used in asynchronous job code: that would disable the worker threads associated with some CPU cores, leading to a
   -- loss of CPU efficiency or even to deadlocks.
   procedure Wait_Completion (Who : Outcome_Client) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- If the asynchronous operation failed with an error, Wait_Completion will re-raise the corresponding exception in
   -- the client thread. If the asynchronous operation was canceled, the following exception will be raised instead:
   Operation_Canceled : exception;

   -- Finally, the completion of an asynchronous operation may be signaled asynchronously to any number of listener
   -- objects. These objects are conceptually similar to the callbacks used by other asynchronous libraries, however
   -- they have the additional ability to retain state, which makes it much easier to build context-aware listeners, or
   -- to hook multiple outcome objects into a single listener in order to aggregate their completion signals.
   --
   -- The flip side to this flexibility is that listener objects should generally be made thread-safe (e.g. by
   -- implementing them as Ada protected objects), unless it can be proven that they will never be accessed concurrently
   -- by multiple OS threads. In addition, because outcome objects must be able to retain a reference to their
   -- listeners, these objects will usually need to be heap allocated and accessed by (smart-)reference.
   --
   -- The current implementation of outcome objects makes it difficult to identify which outcome object is emitting a
   -- signal to a listener. But this feature can be added to the listener interface later if a use case for it emerges.
   --
   type Outcome_Listener_Reference is interface;
   procedure Notify_Outcome (Where : in out Outcome_Listener_Reference;
                             What  : Final_Outcome_Status) is abstract;

   -- Listener objects are bound to outcome objects using the following outcome object method.
   procedure Add_Listener (Where : in out Outcome_Client;
                           Who   : in out Outcome_Listener_Reference'Class) is abstract
     with Pre'Class => (not Where.Is_Null);


   -- === SERVER-SPECIFIC INTERFACE ===

   -- There is only one outcome server per outcome object. This server is owned by the Ada task that is in charge of
   -- executing the asynchronous operation, or of monitoring it if it is being carried out by a third party asynchronous
   -- library. The outcome server can be used to set the final outcome of an asynchronous operation.
   type Outcome_Server is limited interface and Outcome_Reference;

   -- Outcome objects are created by creating the associated outcome server
   function Make_Outcome return Outcome_Server is abstract
     with Post'Class => (not Make_Outcome'Result.Is_Null);

   -- Using an outcome server, one can mark the operation as having completed normally...
   procedure Mark_Done (Who : in out Outcome_Server) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- ...or as having aborted as the result of an Ada exception
   procedure Mark_Error (Who  : in out Outcome_Server;
                         What : Ada.Exceptions.Exception_Occurrence) is abstract
     with Pre'Class => (not Who.Is_Null);

   -- In addition, an outcome server is informed of whether the associated asynchronous operation has been canceled.
   -- For now, only a nonblocking polling interface is provided to this end. We may also provide a way to handle
   -- cancelation using Asynchronous Transfer of Control in the future, but that cancelation mechanism is more difficult
   -- to implement and its effectiveness is Ada implementation-dependent, so we'll need a clear use case for it.
   function Is_Canceled (Who : Outcome_Server) return Boolean is abstract
     with Pre'Class => (not Who.Is_Null);

end Phalanstery.Outcomes.Interfaces;
