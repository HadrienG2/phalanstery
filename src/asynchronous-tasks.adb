with Ada.Assertions;
with Asynchronous.Events.Servers;
with Asynchronous.Utilities.Testing;
pragma Elaborate_All (Asynchronous.Utilities.Testing);

package body Asynchronous.Tasks is

   function Return_Waiting (Cause : Valid_Event_Client) return Return_Value is
     ((State => Waiting, Wait_List_Length => 1, Wait_List => (1 => Cause)));

   function Return_Waiting (Cause : Event_Wait_List) return Return_Value is
     ((State => Waiting, Wait_List_Length => Cause'Length, Wait_List => Cause));

   function Status (What : Return_Value) return Return_Status is
     (What.State);

   function Wait_List (What : Return_Value) return Event_Wait_List is
     (What.Wait_List);


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use type Valid_Event_Client;
      subtype Valid_Event_Server is Events.Contracts.Valid_Event_Server;

      procedure Test_Finished is
      begin
         Assert_Truth (Check   => (Status (Return_Finished) = Finished),
                       Message => "The status of a task returning Return_Finished should be Finished");
         begin
            declare
               Unused : constant Event_Wait_List := Wait_List (Return_Finished) with Unreferenced;
            begin
               Fail ("Querying the wait list of a finished task should be an error");
            end;
         exception
            when Ada.Assertions.Assertion_Error | Constraint_Error =>
               null;
         end;
      end Test_Finished;

      procedure Test_Yielding is
      begin
         Assert_Truth (Check   => (Status (Return_Yielding) = Yielding),
                       Message => "The status of a task returning Return_Yielding should be Yielding");
         begin
            declare
               Unused : constant Event_Wait_List := Wait_List (Return_Yielding) with Unreferenced;
            begin
               Fail ("Querying the wait list of a yielding task should be an error");
            end;
         exception
            when Ada.Assertions.Assertion_Error | Constraint_Error =>
               null;
         end;
      end Test_Yielding;

      procedure Test_Waiting_One is
         E : constant Valid_Event_Server := Events.Servers.Make_Event;
         C : constant Valid_Event_Client := E.Make_Client;
         R : constant Return_Value := Return_Waiting (C);
      begin
         Assert_Truth (Check   => (Status (R) = Waiting),
                       Message => "The status of a waiting task should be Waiting");
         declare
            List : constant Event_Wait_List := Wait_List (R);
         begin
            Assert_Truth (Check   => ((List'Length = 1) and then (List (List'First) = C)),
                          Message => "The event wait list of a waiting task should be correct");
         end;
      end Test_Waiting_One;

      procedure Test_Waiting_Multiple is
         E1, E2 : constant Valid_Event_Server := Events.Servers.Make_Event;
         C1 : constant Valid_Event_Client := E1.Make_Client;
         C2 : constant Valid_Event_Client := E2.Make_Client;
         R : constant Return_Value := Return_Waiting ((C1, C2));
      begin
         Assert_Truth (Check   => (Status (R) = Waiting),
                       Message => "The status of a waiting task should be Waiting");
         declare
            List : constant Event_Wait_List := Wait_List (R);
         begin
            Assert_Truth (Check   => ((List'Length = 2) and then
                                        ((List (List'First) = C1) and (List (List'Last) = C2))),
                          Message => "The event wait list of a waiting task should be correct");
         end;
      end Test_Waiting_Multiple;

      procedure Test_Canceled is
      begin
         Assert_Truth (Check   => (Status (Return_Canceled) = Canceled),
                       Message => "The status of a task returning Return_Canceled should be Canceled");
         begin
            declare
               Unused : constant Event_Wait_List := Wait_List (Return_Canceled) with Unreferenced;
            begin
               Fail ("Querying the wait list of a canceled task should be an error");
            end;
         exception
            when Ada.Assertions.Assertion_Error | Constraint_Error =>
               null;
         end;
      end Test_Canceled;

   begin
      Test_Finished;
      Test_Yielding;
      Test_Waiting_One;
      Test_Waiting_Multiple;
      Test_Canceled;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Asynchronous.Tasks;
