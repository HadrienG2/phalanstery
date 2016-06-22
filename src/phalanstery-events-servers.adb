with Phalanstery.Events.Callbacks;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Utilities.Testing);

package body Phalanstery.Events.Servers is

   use type Implementation.Event_Reference;

   overriding function Is_Null (Who : Server) return Boolean is (Who.Ref.Is_Null);

   overriding function "=" (A, B : Server) return Boolean is (A.Ref = B.Ref);

   overriding procedure Mark_Done (Who : in out Server) is
   begin
      Who.Ref.Set.Mark_Done;
   end Mark_Done;

   overriding procedure Mark_Error (Who  : in out Server;
                                    What : Ada.Exceptions.Exception_Occurrence) is
   begin
      Who.Ref.Set.Mark_Error (What);
   end Mark_Error;

   overriding procedure Cancel (Who : in out Server) is
   begin
      Who.Ref.Set.Cancel;
   end Cancel;

   overriding function Is_Canceled (Who : Server) return Boolean is
     (Who.Ref.Get.Is_Canceled);

   function Make_Event return Server is ((Ref => Implementation.Make_Event));

   function Make_Client (From : Server) return Clients.Client is
     (Clients.Make_Client (From.Ref));


   -- The remainder of this package is dedicated to unit tests
   Test_Callback_Calls : Natural := 0;
   Last_Status : Interfaces.Finished_Event_Status;

   procedure Test_Callback (Final_Status : Interfaces.Finished_Event_Status) is
   begin
      Test_Callback_Calls := Test_Callback_Calls + 1;
      Last_Status := Final_Status;
   end Test_Callback;

   procedure Run_Tests is

      use Utilities.Testing;
      use all type Interfaces.Event_Status;
      use type Ada.Exceptions.Exception_Id;
      use type Clients.Client;

      Test_Error : Ada.Exceptions.Exception_Occurrence;
      Test_Callback_Listener : Callbacks.Callback_Listener := Callbacks.Make_Callback_Listener (Test_Callback'Access);

      procedure Test_Creation is
         C : Clients.Client;
      begin
         declare
            S1 : Servers.Server;
         begin
            Assert_Truth (Check   => (S1.Is_Null and C.Is_Null),
                          Message => "Event servers and clients should be null at creation time");
         end;

         declare
            S2 : constant Servers.Server := Make_Event;
         begin
            Assert_Truth (Check   => (not S2.Is_Null),
                          Message => "Make_Event should return non-null events");

            C := S2.Make_Client;
            Assert_Truth (Check   => (not C.Is_Null),
                          Message => "Make_Client should return non-null events");
         end;
      end Test_Creation;

      procedure Test_Equality is
         C1, C2 : Clients.Client;
      begin
         declare
            S1, S2 : Servers.Server;
         begin
            Assert_Truth (Check   => ((S1 = S1) and (S1 = S2) and (C1 = C1) and (C1 = C2)),
                          Message => "Events in the null state should be considered equal");
         end;

         declare
            S1, S2 : constant Servers.Server := Make_Event;
         begin
            Assert_Truth (Check   => (S1 /= S2),
                          Message => "Independently created servers should be identified as distinct");

            C1 := S1.Make_Client;
            Assert_Truth (Check   => (C1 /= C2),
                          Message => "Null and non-null events should be identified as distinct");

            Assert_Truth (Check   => (S1 = S1) and (C1 = C1),
                          Message => "Initialized servers and clients should be considered equal to themselves");

            C2 := S2.Make_Client;
            Assert_Truth (Check   => (C1 /= C2),
                          Message => "Independently create clients should also be identified as distinct");

            C2 := C1;
            Assert_Truth (Check   => (C1 = C2),
                          Message => "Explicitly assigned events should be considered equal.");


         end;
      end Test_Equality;

      procedure Test_Pending_State is
         S : constant Servers.Server := Make_Event;
         C : Clients.Client := S.Make_Client;
      begin
         Assert_Truth (Check   => (C.Status = Pending),
                       Message => "Clients should initially be in the Pending state");

         C.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Clients should initially hold no error");

         -- NOTE : Non-blocking test of Wait_Completion's blocking-ness may only be done for the Event implementation !

         C.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 0),
                       Message => "Listeners should not be fired in the initial client state");

         Assert_Truth (Check   => not S.Is_Canceled,
                       Message => "Newly created events should not be canceled");
      end Test_Pending_State;

      procedure Test_Done_State is
         S : Servers.Server := Make_Event;
         C : Clients.Client := S.Make_Client;
      begin
         C.Add_Listener (Test_Callback_Listener);
         S.Mark_Done;
         Assert_Truth (Check   => (C.Status = Done),
                       Message => "After marking a server Done, the client should be Done as well");

         C.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                       Message => "Done clients should hold no error");

         -- NOTE : Non-blocking test of Wait_Completion's blocking-ness may only be done for the Event implementation !

         Assert_Truth (Check   => (Test_Callback_Calls = 1) and (Last_Status = Done),
                       Message => "Client callbacks should be fired when a client is marked Done");

         C.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 2) and (Last_Status = Done),
                       Message => "Client callbacks should still be fired after a client is marked Done");

         Assert_Truth (Check   => not S.Is_Canceled,
                       Message => "Done events should not be canceled");

         -- NOTE : Done state perennity is only checked for the Event implementation

         Test_Callback_Calls := 0;
      end Test_Done_State;

      procedure Test_Canceled_State is

         procedure Test_Cancelation (S : Servers.Server;
                                     C : in out Clients.Client) is
         begin
            Assert_Truth (Check   => (C.Status = Canceled),
                          Message => "After canceling a client, it should be marked Canceled");

            C.Get_Error (Test_Error);
            Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Ada.Exceptions.Null_Id),
                          Message => "Canceled clients should hold no error");

            -- NOTE : Non-blocking test of Wait_Completion may only be done for the Event implementation !

            Assert_Truth (Check   => (Test_Callback_Calls = 1) and (Last_Status = Canceled),
                          Message => "Client callbacks should be fired when an event is canceled");

            C.Add_Listener (Test_Callback_Listener);
            Assert_Truth (Check   => (Test_Callback_Calls = 2) and (Last_Status = Canceled),
                          Message => "Client callbacks should still be fired after an event is canceled");

            Assert_Truth (Check   => S.Is_Canceled,
                          Message => "Canceled events should not be seen as such by the server");

            -- NOTE : Canceled state perennity is only checked for the Event implementation

            Test_Callback_Calls := 0;
         end Test_Cancelation;

      begin
         declare
            S : constant Servers.Server := Make_Event;
            C : Clients.Client := S.Make_Client;
         begin
            C.Add_Listener (Test_Callback_Listener);
            C.Cancel;
            Test_Cancelation (S, C);
         end;

         declare
            S : Servers.Server := Make_Event;
            C : Clients.Client := S.Make_Client;
         begin
            C.Add_Listener (Test_Callback_Listener);
            S.Cancel;
            Test_Cancelation (S, C);
         end;
      end Test_Canceled_State;

      procedure Test_Error_State is
         Custom_Error : exception;
         Custom_Error_Occurence : Ada.Exceptions.Exception_Occurrence;
         S : Servers.Server := Make_Event;
         C : Clients.Client := S.Make_Client;
      begin
         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurence);
         C.Add_Listener (Test_Callback_Listener);
         S.Mark_Error (Custom_Error_Occurence);
         Assert_Truth (Check   => (C.Status = Error),
                       Message => "Event server errors should propagate to the client");

         C.Get_Error (Test_Error);
         Assert_Truth (Check   => (Ada.Exceptions.Exception_Identity (Test_Error) = Custom_Error'Identity),
                       Message => "Errors should be propagated correctly to the event client");

         -- NOTE : Non-blocking test of Wait_Completion's blocking-ness may only be done for the Event implementation !

         Assert_Truth (Check   => (Test_Callback_Calls = 1) and (Last_Status = Error),
                       Message => "Client callbacks should be fired when an event is aborted");

         C.Add_Listener (Test_Callback_Listener);
         Assert_Truth (Check   => (Test_Callback_Calls = 2) and (Last_Status = Error),
                       Message => "Client callbacks should still be fired after an event is aborted");

         Assert_Truth (Check   => not S.Is_Canceled,
                       Message => "Erronerous events should not be considered as canceled");

         -- NOTE : Error state perennity is only checked for the Event implementation

         Test_Callback_Calls := 0;
      end Test_Error_State;

   begin
      Test_Creation;
      Test_Equality;
      Test_Pending_State;
      Test_Done_State;
      Test_Canceled_State;
      Test_Error_State;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Events.Servers;
