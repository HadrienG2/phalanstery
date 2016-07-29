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

with Ada.Containers;
with Ada.Exceptions;
with Phalanstery.Asynchronous_Jobs;
with Phalanstery.Examples.Trivial_Jobs;
with Phalanstery.Outcomes.Servers;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Exceptions,
                      Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.Scheduling is

   use all type Outcomes.Interfaces.Outcome_Status;

   Dependency_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

   procedure Schedule_Job (Who   : Valid_Job_Instance;
                           After : Interfaces.Valid_Outcome_Client;
                           On    : Valid_Job_Queue) is
   begin
      case After.Status is
         when Final_Outcome_Status =>
            Schedule_Ready_Job (Who          => Who,
                                According_To => After.Status,
                                On           => On);
         when Pending =>
            On.Set.Waiting.Add_Job;
            declare
               Scheduled_Run : Scheduled_Job := (Instance => Who, Target_Queue => On);
            begin
               After.Add_Listener (Scheduled_Run);
            end;
      end case;
   end Schedule_Job;

   procedure Schedule_Ready_Job (Who          : Valid_Job_Instance;
                                 According_To : Final_Outcome_Status;
                                 On           : Valid_Job_Queue) is
      subtype Aborted_Outcome_Status is Outcomes.Interfaces.Aborted_Outcome_Status;
   begin
      case According_To is
         when Done =>
            On.Set.Ready.Enqueue (Who);
         when Aborted_Outcome_Status =>
            Who.Set.Job_Object.Handle_Aborted_Dependency (According_To);
            case Aborted_Outcome_Status (According_To) is
               when Canceled =>
                  Who.Set.Outcome.Cancel;
               when Error =>
                  Who.Set.Outcome.Mark_Error (Dependency_Error_Occurence);
            end case;
      end case;
   end Schedule_Ready_Job;

   overriding procedure Notify_Outcome (Where : in out Scheduled_Job;
                                        What  : Final_Outcome_Status) is
   begin
      Schedule_Ready_Job (Who          => Where.Instance,
                          According_To => What,
                          On           => Where.Target_Queue);
      Where.Target_Queue.Set.Waiting.Remove_Job;
   end Notify_Outcome;


   -- The remainder of this package is dedicated to unit tests
   subtype Outcome_Status is Outcomes.Interfaces.Outcome_Status;

   Last_Dependency_Failure : Outcome_Status := Pending;

   type Null_Job_With_Handler is new Examples.Trivial_Jobs.Null_Job with null record;

   overriding procedure Handle_Aborted_Dependency (Who               : in out Null_Job_With_Handler;
                                        Dependency_Status : Outcomes.Interfaces.Aborted_Outcome_Status) is
      pragma Unreferenced (Who);
   begin
      Last_Dependency_Failure := Dependency_Status;
   end Handle_Aborted_Dependency;

   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Containers.Count_Type;
      use type Valid_Job_Instance;

      subtype Nullable_Job_Instance is Job_Instances.References.Reference;

      T : Null_Job_With_Handler;
      Queue : Valid_Job_Queue;

      procedure Test_Pending_Wait_List is
         Instance : constant Valid_Job_Instance := Job_Instances.References.Make_Job_Instance (T);
         New_Instance : Nullable_Job_Instance;
         Server : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Client : constant Interfaces.Valid_Outcome_Client := Server.Make_Client;
         Instance_Client : constant Interfaces.Valid_Outcome_Client := Instance.Get.Outcome.Make_Client;
      begin

         Schedule_Job (Who   => Instance,
                       After => Client,
                       On    => Queue);
         Assert_Truth (Check   => (Last_Dependency_Failure = Pending),
                       Message => "Job error handlers should not be invoked when dependencies are only pending");
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 0),
                       Message => "Waiting jobs should not be put on the ready queue");
         Assert_Truth (Check   => (not Queue.Get.Waiting.No_Waiting_Job),
                       Message => "Waiting jobs should be accounted as appropriate");

         Server.Mark_Done;
         Assert_Truth (Check   => (Last_Dependency_Failure = Pending),
                       Message => "Job error handlers should not be invoked when dependencies complete normally");
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 1),
                       Message => "Jobs should go to the ready queue when ready");
         Assert_Truth (Check   => Queue.Get.Waiting.No_Waiting_Job,
                       Message => "Jobs should not remain on the waiting queue after becoming ready.");
         Assert_Truth (Check   => (Instance_Client.Status = Pending),
                       Message => "Pending jobs should still be pending after being enqueued");

         Queue.Set.Ready.Dequeue (New_Instance);
         Assert_Truth (Check   => (New_Instance = Instance),
                       Message => "Job instances should not be modified by the scheduling process");

      end Test_Pending_Wait_List;

      procedure Test_Finished_Wait_List is
         Instance : constant Valid_Job_Instance := Job_Instances.References.Make_Job_Instance (T);
         Server : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Client : constant Interfaces.Valid_Outcome_Client := Server.Make_Client;
         New_Instance : Nullable_Job_Instance;
      begin

         Server.Mark_Done;
         Schedule_Job (Who   => Instance,
                       After => Client,
                       On    => Queue);
         Assert_Truth (Check   => (Last_Dependency_Failure = Pending),
                       Message => "Job error handlers should not be invoked when dependencies complete normally");
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 1),
                       Message => "Jobs waiting for a finished operation should be put on the ready queue immediately");
         Assert_Truth (Check   => Queue.Get.Waiting.No_Waiting_Job,
                       Message => "Ready jobs should not be marked as waiting");


         Queue.Set.Ready.Dequeue (New_Instance);

      end Test_Finished_Wait_List;

      procedure Test_Canceled_Wait_List is
         Instance : constant Valid_Job_Instance := Job_Instances.References.Make_Job_Instance (T);
         Client : Interfaces.Valid_Outcome_Client := Outcomes.Servers.Make_Outcome.Make_Client;
      begin
         Client.Cancel;
         Schedule_Job (Who   => Instance,
                       After => Client,
                       On    => Queue);
         Assert_Truth (Check   => (Last_Dependency_Failure = Canceled),
                       Message => "Job error handlers should be invoked when job dependencies are canceled");
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 0),
                       Message => "Jobs waiting for a canceled operation should not be enqueued in the job queue");
         Assert_Truth (Check   => Queue.Get.Waiting.No_Waiting_Job,
                       Message => "Canceled jobs should not be marked as waiting");
         Assert_Truth (Check   => Instance.Get.Outcome.Is_Canceled,
                       Message => "Jobs waiting for canceled operations should be marked canceled");
      end Test_Canceled_Wait_List;

      procedure Test_Erronerous_Wait_List is
         Custom_Error : exception;
         Custom_Error_Occurence : Ada.Exceptions.Exception_Occurrence;
         Instance : constant Valid_Job_Instance := Job_Instances.References.Make_Job_Instance (T);
         Server : Interfaces.Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
         Client : constant Interfaces.Valid_Outcome_Client := Server.Make_Client;
         Instance_Client : constant Interfaces.Valid_Outcome_Client := Instance.Get.Outcome.Make_Client;
      begin

         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurence);
         Server.Mark_Error (Custom_Error_Occurence);
         Schedule_Job (Who   => Instance,
                       After => Client,
                       On    => Queue);
         Assert_Truth (Check   => (Last_Dependency_Failure = Error),
                       Message => "Job error handlers should be invoked when job dependencies fail");
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 0),
                       Message => "Jobs waiting for erronerous operations should not be enqueued in the job queue");
         Assert_Truth (Check   => Queue.Get.Waiting.No_Waiting_Job,
                       Message => "Erronerous jobs should not be marked as waiting");
         Assert_Truth (Check   => (Instance_Client.Status = Error),
                       Message => "Jobs waiting for erronerous operations should be marked erronerous");

         declare
            Test_Error : Ada.Exceptions.Exception_Occurrence;
         begin
            Instance_Client.Get_Error (Test_Error);
            Assert_Truth (Check   => Utilities.Exceptions.Is_Occurrence_Of (
                                        Who  => Test_Error,
                                        What => Asynchronous_Jobs.Dependency_Error'Identity
                                     ),
                          Message => "Job waiting for erronerous operations should be marked with the right error");
         end;

      end Test_Erronerous_Wait_List;

   begin
      Test_Pending_Wait_List;
      Test_Finished_Wait_List;
      Test_Canceled_Wait_List;
      Test_Erronerous_Wait_List;
   end Run_Tests;

begin

   -- Save an occurence of Error_In_Wait_List, to be propagated as needed
   Utilities.Exceptions.Make_Occurrence (What  => Asynchronous_Jobs.Dependency_Error'Identity,
                                         Where => Dependency_Error_Occurence);

   -- Conditionally run the unit tests on startup
   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.Scheduling;
