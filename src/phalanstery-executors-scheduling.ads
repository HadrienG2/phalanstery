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
with Phalanstery.Executors.Job_Instances.References;
with Phalanstery.Executors.Job_Queues.References;
with Phalanstery.Outcomes.Interfaces;

private package Phalanstery.Executors.Scheduling is

   -- This package implements the job scheduling logic of Phalanstery executors. By using it, one can schedule an
   -- asynchronous job to run after some other asynchronous operations have completed, handling possible complications
   -- such as dependency errors along the way.

   -- First, let's define some convenience notations
   subtype Valid_Job_Instance is Job_Instances.References.Valid_Reference;
   subtype Valid_Job_Queue is Job_Queues.References.Valid_Reference;

   -- This function schedules a job instance to be put on the ready queue of an executor, and thus ultimately run, after
   -- the asynchronous operations that it depends on have completed. If some waiting is needed, the waiting counter of
   -- the job queue will be incremented and decremented as appropriate.
   procedure Schedule_Job (Who   : Valid_Job_Instance;
                           After : Interfaces.Valid_Outcome_Client;
                           On    : Valid_Job_Queue);

private

   -- Let us define one more shorthand for the private part of this package
   subtype Final_Outcome_Status is Outcomes.Interfaces.Final_Outcome_Status;

   -- Now we can define what will happen once a job's dependencies are satisfied or have failed
   procedure Schedule_Ready_Job (Who          : Valid_Job_Instance;
                                 According_To : Final_Outcome_Status;
                                 On           : Valid_Job_Queue);

   -- The method above will be invoked by the following outcome listener...
   type Scheduled_Job is new Outcomes.Interfaces.Outcome_Listener_Reference with
      record
         Instance : Valid_Job_Instance;
         Target_Queue : Valid_Job_Queue;
      end record;

   -- ...equipped with its traditional outcome object callback
   overriding procedure Notify_Outcome (Where : in out Scheduled_Job;
                                        What  : Final_Outcome_Status);

end Phalanstery.Executors.Scheduling;
