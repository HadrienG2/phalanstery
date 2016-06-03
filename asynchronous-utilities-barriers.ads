package Asynchronous.Utilities.Barriers is

   -- The signal primitive is intended as a lightweight way for one task to wait for N tasks do something.
   -- A typical use case for it is task shutdown in scenarios where terminate alternatives cannot be used.
   protected type Barrier (Number_Of_Tasks : Natural) is
      procedure Join;
      entry Wait;
   private
      Ready_Tasks : Natural := 0;
   end Barrier;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Asynchronous.Utilities.Barriers;
