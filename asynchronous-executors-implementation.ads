with Asynchronous.Executors.Interfaces;

package Asynchronous.Executors.Implementation is

   -- This is the definition of an asynchronous executor task, the underlying implementation behind executor objects.
   task type Executor_Task (Number_Of_Workers : Interfaces.Worker_Count) is

      -- Queue a new asynchronous task
      entry Schedule_Task (What  : Interfaces.Any_Async_Task;
                           After : Interfaces.Event_Wait_List;
                           Event : out Interfaces.Event_Client);

      -- Schedule executor termination
      entry Stop;

   end Executor_Task;

end Asynchronous.Executors.Implementation;
