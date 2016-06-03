with Asynchronous.Tasks;
with Events.Clients;
with System.Multiprocessors;

package Asynchronous.Executors.Interfaces is

   -- Let us define some common convenience notations for executors
   package Async_Tasks renames Asynchronous.Tasks;
   subtype Any_Async_Task is Async_Tasks.Async_Task'Class;
   subtype Event_Client is Events.Clients.Client;
   subtype Event_Wait_List is Async_Tasks.Event_Wait_List;

   -- Errors in an asynchronous task's wait list will be propagated to the output event with a special exception.
   Error_In_Wait_List : exception;

   -- Executors use a configurable amount of worker threads under the hood. A good rule of thumb is to spawn as many
   -- workers as there are CPU threads, but the optimal amount of workers varies depending on the problem at hand.
   subtype Worker_Count is System.Multiprocessors.CPU;
   Hardware_Workers : constant Worker_Count := System.Multiprocessors.Number_Of_CPUs;

   -- The interface to executors is defined as follows
   type Executor is limited interface;

   -- Schedule a task, do not care when it will run
   procedure Schedule_Task (Where : in out Executor;
                            What : Any_Async_Task) is abstract;

   -- Schedule a task which waits for one event, do not synchronize
   procedure Schedule_Task (Where : in out Executor;
                            What  : Any_Async_Task;
                            After : Event_Client) is abstract;

   -- Schedule a task which waits for multiple events, do not synchronize
   procedure Schedule_Task (Where : in out Executor;
                            What  : Any_Async_Task;
                            After : Event_Wait_List) is abstract;

   -- Schedule a task immediately, get an event to synchronize on
   function Schedule_Task (Where : in out Executor;
                           What : Any_Async_Task) return Event_Client is abstract;

   -- Schedule a task which waits for one event, get an event to synchronize on
   function Schedule_Task (Where : in out Executor;
                           What  : Any_Async_Task;
                           After : Event_Client) return Event_Client is abstract;

   -- Schedule a tasks which waits for multiple events, get an event to synchronize on
   function Schedule_Task (Where : in out Executor;
                           What  : Any_Async_Task;
                           After : Event_Wait_List) return Event_Client is abstract;

end Asynchronous.Executors.Interfaces;
