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

with Phalanstery.Events.Contracts;
with Phalanstery.Jobs;
with System.Multiprocessors;

package Phalanstery.Executors.SMP.Interfaces is

   -- Let us define some common convenience notations for executors
   subtype Any_Async_Job is Jobs.Async_Job'Class;
   subtype Valid_Event_Client is Events.Contracts.Valid_Event_Client;
   subtype Event_Wait_List is Jobs.Event_Wait_List;

   -- Errors in an asynchronous job's wait list will be propagated to the output event with a special exception.
   Error_In_Wait_List : exception;

   -- Executors use a configurable amount of worker threads under the hood. A good rule of thumb is to spawn as many
   -- workers as there are CPU threads, but the optimal amount of workers varies depending on the problem at hand.
   subtype Worker_Count is System.Multiprocessors.CPU;
   Hardware_Workers : constant Worker_Count := System.Multiprocessors.Number_Of_CPUs;

   -- The interface to executors is defined as follows
   type Executor is limited interface;

   -- Schedule a job, do not care when it will run
   procedure Schedule_Job (Where : in out Executor;
                           What : Any_Async_Job) is abstract;

   -- Schedule a job which waits for one event, do not synchronize
   procedure Schedule_Job (Where : in out Executor;
                           What  : Any_Async_Job;
                           After : Valid_Event_Client) is abstract;

   -- Schedule a job which waits for multiple events, do not synchronize
   procedure Schedule_Job (Where : in out Executor;
                           What  : Any_Async_Job;
                           After : Event_Wait_List) is abstract;

   -- Schedule a job immediately, get an event to synchronize on
   function Schedule_Job (Where : in out Executor;
                          What : Any_Async_Job) return Valid_Event_Client is abstract;

   -- Schedule a job which waits for one event, get an event to synchronize on
   function Schedule_Job (Where : in out Executor;
                          What  : Any_Async_Job;
                          After : Valid_Event_Client) return Valid_Event_Client is abstract;

   -- Schedule a jobs which waits for multiple events, get an event to synchronize on
   function Schedule_Job (Where : in out Executor;
                          What  : Any_Async_Job;
                          After : Event_Wait_List) return Valid_Event_Client is abstract;

end Phalanstery.Executors.SMP.Interfaces;
