with Asynchronous.Executors.Interfaces;
with Asynchronous.Executors.Task_Instances.References;
with Asynchronous.Executors.Task_Queues.References;

private package Asynchronous.Executors.Scheduling is

   -- Let us define some convenience notations first
   subtype Valid_Task_Instance_Reference is Task_Instances.References.Valid_Reference;
   subtype Valid_Task_Queue_Reference is Task_Queues.References.Valid_Reference;

   -- This function handles blocking tasks by allowing a task instance to be scheduled for execution (through queueing
   -- on an executor's ready task queue) after an event wait list has been completed. It also handles all the non-Done
   -- statuses which the wait list can end up in, allowing for proper handling of cancelation and errors.
   procedure Schedule_Task (Who   : Valid_Task_Instance_Reference;
                            After : Interfaces.Event_Wait_List;
                            On    : Valid_Task_Queue_Reference);

end Asynchronous.Executors.Scheduling;
