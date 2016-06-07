package Asynchronous.Executors with Pure is

   -- To execute asynchronous tasks, one must send them to executor objects. These will take care of running the task.
   -- In the future, executors might span multiple physical machines and perform distributed load balancing.
   --
   -- The children of this package are organized as follows :
   --    - Executors.Interfaces defines the common interface design followed by executors.
   --    - Executors.Task_Instances defines a way to package running asynchronous tasks.
   --    - Executors.Task_Queues defines data structures for the manadement of pending tasks.
   --    - Executors.Scheduling defines the task scheduling logic of executors.
   --    - Executors.Executor_Tasks provides a task-based implementation of the executor concept.
   --    - Executors.Objects provides a high-level interface to executor tasks

end Asynchronous.Executors;
