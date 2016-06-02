package Asynchronous.Executors is

   -- To execute asynchronous tasks, one must send them to executor objects. These will take care of running the task.
   -- In the future, executors might span multiple physical machines and perform distributed load balancing.
   --
   -- The children of this package are organized as follows :
   --    - Asynchronous.Executors.Interfaces defines the common interface design followed by executors.
   --    - Asynchronous.Executors.Implementation features a task-based implementation of the executor concept.
   --    - Asynchronous.Executors.Objects features an object encapsulating the details of executor task maintenance.

end Asynchronous.Executors;
