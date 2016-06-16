with Asynchronous.Events.Composition;
with Asynchronous.Events.Contracts;

package Asynchronous.Tasks is

   -- An asynchronous task is defined as a user-specified, event-driven cooperative multitasking work item.

   -- When I say that asynchronous tasks are event-driven, I mean that they may wait for events to occur, and in general
   -- do so from the start. Thus, we need a data structures representing a list of events for tasks to wait on.
   --
   subtype Valid_Event_Client is Events.Contracts.Valid_Event_Client;
   subtype Event_Wait_List is Events.Composition.Valid_Event_List;

   -- On every run, a task returns status information to the underlying task scheduler.
   -- If a task cannot complete normally, it should raise an exception instead, and the runtime will handle it.
   type Return_Status is (Finished, Yielding, Waiting, Canceled);
   type Return_Value (<>) is private;

   -- Task return values are created in the following way...
   Return_Finished : constant Return_Value;
   Return_Yielding : constant Return_Value;
   Return_Canceled : constant Return_Value;
   function Return_Waiting (Cause : Valid_Event_Client) return Return_Value;
   function Return_Waiting (Cause : Event_Wait_List) return Return_Value;

   -- ...and queried in the following way
   function Status (What : Return_Value) return Return_Status;
   function Wait_List (What : Return_Value) return Event_Wait_List
     with Pre => (Status (What) = Waiting);

   -- Asynchronous tasks are user-defined, by inheriting from a common interface.
   --
   -- One thing to keep in mind is that for task queueing and load balancing to be efficient, you should design task
   -- types to be cheap to copy, for example by having them host pointers or references to arrays instead of raw arrays.
   --
   -- Another important consideration is that in order to follow Ada's accessibility rules, task types should currently
   -- be defined at global scope. The reason is that asynchronous task objects might otherwise outlive their type.
   --
   -- Task cancelation is handled as follows: dependents of the canceled task are canceled immediately, and the task
   -- itself gets notified about the cancelation through a parameter to Run. A task may also choose to cancel itself,
   -- along with its dependents, by returning Return_Canceled.
   --
   type Async_Task is interface;
   function Run (Who          : in out Async_Task;
                 Was_Canceled : Boolean) return Return_Value is abstract;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   type Return_Value (State : Return_Status; Wait_List_Length : Natural) is
      record
         case State is
            when Finished | Yielding | Canceled =>
               null;
            when Waiting =>
               Wait_List : Event_Wait_List (1 .. Wait_List_Length);
         end case;
      end record;

   Return_Finished : constant Return_Value := (State => Finished, Wait_List_Length => 0);
   Return_Yielding : constant Return_Value := (State => Yielding, Wait_List_Length => 0);
   Return_Canceled : constant Return_Value := (State => Canceled, Wait_List_Length => 0);

end Asynchronous.Tasks;
