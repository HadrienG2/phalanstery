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
with Phalanstery.Events.Composition.Shortcuts;
with Phalanstery.Events.Contracts;
with Phalanstery.Events.Servers;
with Phalanstery.Jobs;
with Phalanstery.Jobs.Trivial;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Exceptions,
                      Phalanstery.Utilities.Testing);

package body Phalanstery.Executors.Scheduling is

   use all type Events.Interfaces.Event_Status;

   Wait_List_Error_Occurence : Ada.Exceptions.Exception_Occurrence;

   procedure Schedule_Ready_Job (Who          : Valid_Job_Instance_Reference;
                                 According_To : Finished_Event_Status;
                                 On           : Valid_Job_Queue_Reference) is
   begin
      case According_To is
         when Done =>
            On.Set.Ready.Enqueue (Who);
         when Canceled =>
            Who.Set.Completion_Event.Cancel;
         when Error =>
            Who.Set.Completion_Event.Mark_Error (Wait_List_Error_Occurence);
      end case;
   end Schedule_Ready_Job;

   overriding procedure Notify_Event_Status_Change (Where : in out Scheduled_Job;
                                                    What  : Finished_Event_Status) is
   begin
      Schedule_Ready_Job (Who          => Where.Instance,
                          According_To => What,
                          On           => Where.Target_Queue);
      Where.Target_Queue.Set.Pending.Remove_Job;
   end Notify_Event_Status_Change;

   procedure Schedule_Job (Who   : Valid_Job_Instance_Reference;
                           After : Interfaces.Event_Wait_List;
                           On    : Valid_Job_Queue_Reference) is
      Input_Event : Interfaces.Valid_Event_Client := Events.Composition.Shortcuts.When_All (After);
   begin
      case Input_Event.Status is
         when Finished_Event_Status =>
            Schedule_Ready_Job (Who          => Who,
                                According_To => Input_Event.Status,
                                On           => On);
         when Pending =>
            On.Set.Pending.Add_Job;
            declare
               Scheduled : Scheduled_Job := (Instance => Who, Target_Queue => On);
            begin
               Input_Event.Add_Listener (Scheduled);
            end;
      end case;
   end Schedule_Job;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use type Ada.Containers.Count_Type;
      use type Valid_Job_Instance_Reference;
      subtype Valid_Event_Server is Events.Contracts.Valid_Event_Server;

      T : Jobs.Trivial.Null_Job;
      Queue : constant Valid_Job_Queue_Reference;

      procedure Test_Finished_Wait_List is
         Instance : constant Valid_Job_Instance_Reference := Job_Instances.References.Make_Job_Instance (T);
         Empty_List : Interfaces.Event_Wait_List (2 .. 1);
         New_Instance : Job_Instances.References.Reference;
      begin

         Schedule_Job (Who   => Instance,
                       After => Empty_List,
                       On    => Queue);
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 1),
                       Message => "If the event wait list is ready, jobs should be enqueued immediately");
         Assert_Truth (Check   => Queue.Get.Pending.No_Pending_Job,
                       Message => "Ready jobs should not be marked as pending");

         Queue.Set.Ready.Dequeue (New_Instance);
         Assert_Truth (Check   => (New_Instance = Instance),
                       Message => "Scheduling a ready job should enqueue the right job instance");

      end Test_Finished_Wait_List;

      procedure Test_Canceled_Wait_List is
         Instance : constant Valid_Job_Instance_Reference := Job_Instances.References.Make_Job_Instance (T);
         Client : Interfaces.Valid_Event_Client := Events.Servers.Make_Event.Make_Client;
      begin
         Client.Cancel;
         Schedule_Job (Who   => Instance,
                       After => (1 => Client),
                       On    => Queue);
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 0),
                       Message => "Jobs with canceled input events should not be enqueued in the job queue");
         Assert_Truth (Check   => Queue.Get.Pending.No_Pending_Job,
                       Message => "Canceled jobs should not be marked as pending");
         Assert_Truth (Check   => Instance.Get.Completion_Event.Is_Canceled,
                       Message => "Jobs with canceled input events should be marked canceled");
      end Test_Canceled_Wait_List;

      procedure Test_Erronerous_Wait_List is
         Custom_Error : exception;
         Custom_Error_Occurence : Ada.Exceptions.Exception_Occurrence;
         Instance : constant Valid_Job_Instance_Reference := Job_Instances.References.Make_Job_Instance (T);
         Server : Valid_Event_Server := Events.Servers.Make_Event;
         Client : constant Interfaces.Valid_Event_Client := Server.Make_Client;
         Instance_Client : constant Interfaces.Valid_Event_Client := Instance.Get.Completion_Event.Make_Client;
      begin

         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurence);
         Server.Mark_Error (Custom_Error_Occurence);
         Schedule_Job (Who   => Instance,
                       After => (1 => Client),
                       On    => Queue);
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 0),
                       Message => "Jobs with erronerous input events should not be enqueued in the job queue");
         Assert_Truth (Check   => Queue.Get.Pending.No_Pending_Job,
                       Message => "Erronerous jobs should not be marked as pending");
         Assert_Truth (Check   => (Instance_Client.Status = Error),
                       Message => "Jobs with erronerous input events should be marked erronerous");

         declare
            Test_Error : Ada.Exceptions.Exception_Occurrence;
         begin
            Instance_Client.Get_Error (Test_Error);
            Assert_Truth (Check   => Utilities.Exceptions.Is_Occurrence_Of (
                                        Who  => Test_Error,
                                        What => Interfaces.Error_In_Wait_List'Identity
                                     ),
                          Message => "Job with erronerous input events should be marked with the right error");
         end;

      end Test_Erronerous_Wait_List;

      procedure Test_Pending_Wait_List is
         Instance : Valid_Job_Instance_Reference := Job_Instances.References.Make_Job_Instance (T);
         Server : Valid_Event_Server := Events.Servers.Make_Event;
         Client : constant Interfaces.Valid_Event_Client := Server.Make_Client;
         Instance_Client : constant Interfaces.Valid_Event_Client := Instance.Get.Completion_Event.Make_Client;
      begin

         Schedule_Job (Who   => Instance,
                       After => (1 => Client),
                       On    => Queue);
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 0),
                       Message => "Pending jobs should not be put on the ready queue");
         Assert_Truth (Check   => (not Queue.Get.Pending.No_Pending_Job),
                       Message => "Pending jobs should be accounted as appropriate");

         Server.Mark_Done;
         Assert_Truth (Check   => (Queue.Get.Ready.Current_Use = 1),
                       Message => "Jobs should go to the ready queue when ready");
         Assert_Truth (Check   => Queue.Get.Pending.No_Pending_Job,
                       Message => "Jobs should not remain on the pending queue after becoming ready.");
         Assert_Truth (Check   => (Instance_Client.Status = Pending),
                       Message => "Pending jobs should still be pending after being enqueued");

         Queue.Set.Ready.Dequeue (Instance);

      end Test_Pending_Wait_List;

   begin
      Test_Finished_Wait_List;
      Test_Canceled_Wait_List;
      Test_Erronerous_Wait_List;
      Test_Pending_Wait_List;
   end Run_Tests;

begin

   -- Save an occurence of Error_In_Wait_List, to be propagated as needed
   Utilities.Exceptions.Make_Occurrence (What  => Interfaces.Error_In_Wait_List'Identity,
                                         Where => Wait_List_Error_Occurence);

   -- Conditionally run the unit tests on startup
   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Executors.Scheduling;
