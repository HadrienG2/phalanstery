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

with Phalanstery.Executors.Interfaces;
with System.Multiprocessors;

package Phalanstery.Executors.SMP.Specific_Interfaces is

   -- This package holds the part of the executor interface that is specific to SMP operation

   -- Executors use a configurable amount of worker threads under the hood. A good rule of thumb is to spawn as many
   -- workers as there are CPU threads, but the optimal amount of workers varies depending on the problem at hand.
   subtype Worker_Count is System.Multiprocessors.CPU;
   Hardware_Workers : constant Worker_Count := System.Multiprocessors.Number_Of_CPUs;

   -- The interface to executors is defined as follows
   type Executor is limited interface;

   -- Schedule a job, do not care when it will run
   procedure Schedule_Job (Where : in out Executor;
                           What : Interfaces.Any_Asynchronous_Job) is abstract;

   -- Schedule a job which depends on one asynchronous operation, do not synchronize
   procedure Schedule_Job (Where : in out Executor;
                           What  : Interfaces.Any_Asynchronous_Job;
                           After : Interfaces.Valid_Outcome_Client) is abstract;

   -- Schedule a job which depends on multiple operations, do not synchronize
   procedure Schedule_Job (Where : in out Executor;
                           What  : Interfaces.Any_Asynchronous_Job;
                           After : Interfaces.Valid_Outcome_List) is abstract;

   -- Schedule a job immediately, get an outcome object to synchronize on
   function Schedule_Job (Where : in out Executor;
                          What : Interfaces.Any_Asynchronous_Job) return Interfaces.Valid_Outcome_Client is abstract;

   -- Schedule a job which depends on one asynchronous operation, get an outcome object to synchronize on
   function Schedule_Job (Where : in out Executor;
                          What  : Interfaces.Any_Asynchronous_Job;
                          After : Interfaces.Valid_Outcome_Client) return Interfaces.Valid_Outcome_Client is abstract;

   -- Schedule a jobs which depends on multiple operations, get an outcome object to synchronize on
   function Schedule_Job (Where : in out Executor;
                          What  : Interfaces.Any_Asynchronous_Job;
                          After : Interfaces.Valid_Outcome_List) return Interfaces.Valid_Outcome_Client is abstract;

end Phalanstery.Executors.SMP.Specific_Interfaces;
