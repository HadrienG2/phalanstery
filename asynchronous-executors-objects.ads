with Ada.Finalization;
with Asynchronous.Executors.Implementation;
with Asynchronous.Executors.Interfaces;

package Asynchronous.Executors.Objects is

   -- This package defines executor objects, whose mission is to execute asynchronous tasks.
   --
   -- Note that you should NEVER create an executor object at global scope, as that will lead to a deadlock : the
   -- environment task will wait for the executor task to complete before finalizing the executor object, whereas the
   -- termination signal for that task should be sent by the finalization of the executor object.
   --
   type Executor (Number_Of_Workers : Interfaces.Worker_Count := Interfaces.Hardware_Workers) is
     new Ada.Finalization.Limited_Controlled and Interfaces.Executor with private;

   -- Schedule a task, do not care when it will run
   overriding procedure Schedule_Task (Where : in out Executor;
                                       What : Interfaces.Any_Async_Task);

   -- Schedule a task which waits for one event, do not synchronize
   overriding procedure Schedule_Task (Where : in out Executor;
                                       What  : Interfaces.Any_Async_Task;
                                       After : Interfaces.Event_Client);

   -- Schedule a task which waits for multiple events, do not synchronize
   overriding procedure Schedule_Task (Where : in out Executor;
                                       What  : Interfaces.Any_Async_Task;
                                       After : Interfaces.Event_Wait_List);

   -- Schedule a task immediately, get an event to synchronize on
   overriding function Schedule_Task (Where : in out Executor;
                                      What : Interfaces.Any_Async_Task) return Interfaces.Event_Client;

  -- Schedule a task which waits for one event, get an event to synchronize on
   overriding function Schedule_Task (Where : in out Executor;
                                      What  : Interfaces.Any_Async_Task;
                                      After : Interfaces.Event_Client) return Interfaces.Event_Client;

   -- Schedule a tasks which waits for multiple events, get an event to synchronize on
   overriding function Schedule_Task (Where : in out Executor;
                                      What  : Interfaces.Any_Async_Task;
                                      After : Interfaces.Event_Wait_List) return Interfaces.Event_Client;

private

   -- Executor tasks must be heap allocated, because otherwise the Ada runtime's finalizer for Executor will wait for
   -- Executor_Task to terminate, thusly not calling the finalizer of Executor, which would send the Stop signal
   -- to the executor task, which would in turn be needed for that task to stop.

   subtype Executor_Task is Implementation.Executor_Task;

   type Executor_Access is access Executor_Task;

   type Executor (Number_Of_Workers : Interfaces.Worker_Count := Interfaces.Hardware_Workers) is
     new Ada.Finalization.Limited_Controlled and Interfaces.Executor with
      record
         Executor_Task : Executor_Access := null;
      end record;

   overriding procedure Initialize (Who : in out Executor);
   overriding procedure Finalize (Who : in out Executor);

end Asynchronous.Executors.Objects;
