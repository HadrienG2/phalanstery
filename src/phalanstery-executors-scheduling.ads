with Phalanstery.Events.Interfaces;
with Phalanstery.Executors.Interfaces;
with Phalanstery.Executors.Job_Instances.References;
with Phalanstery.Executors.Job_Queues.References;

private package Phalanstery.Executors.Scheduling is

   -- Let us define some convenience notations first
   subtype Valid_Job_Instance_Reference is Job_Instances.References.Valid_Reference;
   subtype Valid_Job_Queue_Reference is Job_Queues.References.Valid_Reference;

   -- This function handles blocking jobs by allowing a job instance to be scheduled for execution (through queueing
   -- on an executor's ready job queue) after an event wait list has been completed. It also handles all the non-Done
   -- statuses which the wait list can end up in, allowing for proper handling of cancelation and errors.
   procedure Schedule_Job (Who   : Valid_Job_Instance_Reference;
                           After : Interfaces.Event_Wait_List;
                           On    : Valid_Job_Queue_Reference);

private

   -- Let us define this conveience notation first
   subtype Finished_Event_Status is Events.Interfaces.Finished_Event_Status;

   -- It is easy to schedule a job given the prior assumption that it's waiting for a finished event of known status
   procedure Schedule_Ready_Job (Who          : Valid_Job_Instance_Reference;
                                 According_To : Finished_Event_Status;
                                 On           : Valid_Job_Queue_Reference);

   -- We wait for non-ready jobs using the following listener object
   type Scheduled_Job is new Events.Interfaces.Event_Listener_Reference with
      record
         Instance : Valid_Job_Instance_Reference;
         Target_Queue : Valid_Job_Queue_Reference;
      end record;

   overriding procedure Notify_Event_Status_Change (Where : in out Scheduled_Job;
                                                    What  : Finished_Event_Status);

end Phalanstery.Executors.Scheduling;
