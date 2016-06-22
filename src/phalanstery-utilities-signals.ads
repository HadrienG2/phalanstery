package Phalanstery.Utilities.Signals is

   -- The signal primitive is intended as a lightweight way for one task to have N tasks do something exactly once
   -- A typical use case for it is task shutdown in scenarios where terminate alternatives cannot be used.
   protected type Signal is
      procedure Send;
      entry Wait;
   private
      Sent : Boolean := False;
   end Signal;

   -- Run the unit tests for this package
   procedure Run_Tests;

end Phalanstery.Utilities.Signals;
