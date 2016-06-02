package Utilities.Testing is

   -- This package provides unit testing facilities which facilitate spotting regressions in the system

   -- This procedure may be used to conditionally run tests for all imported modules on application startup
   procedure Startup_Test (How : access procedure);

   -- This procedure asserts that a boolean predicate is true, else aborts after displaying a rationale
   procedure Assert_Truth (Check   : Boolean;
                           Message : String);

   -- This procedure represents a failed test
   procedure Fail (Message : String);

private

   -- Set this flag to True in order to enable Startup_Test.
   Run_Tests_On_Startup : Boolean := True;

end Utilities.Testing;
