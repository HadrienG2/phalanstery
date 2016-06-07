with Ada.Finalization;
with Asynchronous.Events.Servers;
with Asynchronous.Executors.Interfaces;

package Asynchronous.Executors.Task_Instances is

   -- An asynchronous executor manipulates tasks instances, which are composed of a mutable copy of the source task and
   -- some associated scheduler metadata such as the task's output event.

   -- Task copies must be heap-allocated because we use a class-wide type for them.
   type Task_Access is access Interfaces.Any_Async_Task;

   -- Task instances are currently composed of a task copy and an event used for signaling task completion.
   type Task_Instance is new Ada.Finalization.Limited_Controlled with
      record
         Task_Object : Task_Access := null;
         Completion_Event : Events.Servers.Server := Events.Servers.Make_Event;
      end record;

   -- Because instances must contain pointers, we should make sure that they are always finalized properly
   overriding procedure Finalize (Who : in out Task_Instance);

   -- Because task instances will be moved around, we need some kind of efficiently copyable reference to them.
   -- Due to Ada elaboration technicalities, these references must be implemented in a child package, called References.

end Asynchronous.Executors.Task_Instances;
