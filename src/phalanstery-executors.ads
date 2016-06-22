package Phalanstery.Executors with Pure is

   -- To execute asynchronous jobs, one must send them to executor objects. These will take care of running the job.
   -- In the future, executors might span multiple physical machines and perform distributed load balancing.
   --
   -- The children of this package are organized as follows :
   --    - Executors.Interfaces defines the common interface design followed by executors.
   --    - Executors.Job_Instances defines a way to package running asynchronous jobs.
   --    - Executors.Job_Queues defines data structures for the manadement of pending jobs.
   --    - Executors.Scheduling defines the job scheduling logic of executors.
   --    - Executors.Executor_Tasks provides a task-based implementation of the executor concept.
   --    - Executors.Objects provides a high-level interface to executor tasks

end Phalanstery.Executors;
