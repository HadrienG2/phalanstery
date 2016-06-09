with Asynchronous.Executors.Interfaces;

private package Asynchronous.Executors.Executor_Tasks is

   -- Under the hood, executor objects spawn and manage an Ada task, which is called the executor task.
   task type Executor_Task (Number_Of_Workers : Interfaces.Worker_Count) is

      -- Queue a new asynchronous task
      entry Schedule_Task (What  : Interfaces.Any_Async_Task;
                           After : Interfaces.Event_Wait_List;
                           Event : out Interfaces.Valid_Event_Client);

      -- Schedule executor termination
      entry Stop;

   end Executor_Task;

   -- Run the unit tests for this package
   procedure Run_Tests;

private

   -- Task executors can currently operate according to one of two statically selected scheduling policies:
   --    - In batch mode, executors run tasks as long as they can, which maximizes computational performance.
   --    - In round-robin mode, executors switch between tasks in a cyclic FIFO fashion, which minimizes starvation.
   type Scheduling_Policy is (Batch, Round_Robin);
   Active_Scheduling_Policy : constant Scheduling_Policy := Batch;

end Asynchronous.Executors.Executor_Tasks;
