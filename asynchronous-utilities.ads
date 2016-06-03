package Asynchronous.Utilities is

   -- This package groups together a set of simple components that haven't really found their place anywhere else.
   --    - Utilities.Atomic_Counters is for now an interface to GNAT's System.Atomic_Counters, allowing us to extend it.
   --    - Utilities.Barriers provides a synchronization primitive allowing a task to wait for N others.
   --    - Utilities.Debug provides some debugging tools, particularly related to output.
   --    - Utilities.References provides reference counting facilities, which are needed to address the scoping
   --         complications that arise when objects can asynchronously go out of scope.
   --    - Utilities.Signals provides a synchronization primitive allowing a task to trigger work in N others.
   --    - Utilities.Testing provides testing facilities.

end Asynchronous.Utilities;
