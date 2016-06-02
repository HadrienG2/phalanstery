with Events.Callbacks;
with Utilities.Testing;
pragma Elaborate_All (Utilities.Testing);

package body Events.Servers is

   use type Event_Reference;

   overriding function "=" (A, B : Server) return Boolean is (A.Ref = B.Ref);

   overriding procedure Mark_Done (Who : in out Server) is
   begin
      Who.Ref.Get.Mark_Done;
   end Mark_Done;

   overriding procedure Mark_Error (Who  : in out Server;
                                    What : Ada.Exceptions.Exception_Occurrence) is
   begin
      Who.Ref.Get.Mark_Error (What);
   end Mark_Error;

   overriding procedure Cancel (Who : in out Server) is
   begin
      Who.Ref.Get.Cancel;
   end Cancel;

   overriding function Is_Canceled (Who : Server) return Boolean is
     (Who.Ref.Get.Is_Canceled);

   function Make_Client (From : Server) return Clients.Client is
     (Clients.Make_Client (From.Ref));


   -- The remainder of this package is dedicated to unit tests
   Test_Callback_Calls : Natural := 0;
   Last_Status : Event_Status;

   procedure Test_Callback (Final_Status : Event_Status) is
   begin
      Test_Callback_Calls := Test_Callback_Calls + 1;
      Last_Status := Final_Status;
   end Test_Callback;

   procedure Run_Tests is

      use Events.Callbacks;
      use Utilities.Testing;
      use type Ada.Exceptions.Exception_Id;
      use type Clients.Client;

      Test_Error : Ada.Exceptions.Exception_Occurrence;
      Test_Callback_Listener : Callback_Listener := Make_Callback_Listener (Test_Callback'Access);

      procedure Test_Equality is
         S1, S2 : Servers.Server;
         C1, C2 : Clients.Client;
      begin
         Assert_Truth (Check   => (S1 /= S2) and (C1 /= C2),
                       Message => "Independently created clients and servers should be identified as distinct");

         Assert_Truth (Check   => (S1 = S1) and (C1 = C1),
                       Message => "Servers and clients should be considered equal to themselves");

         C1 := C2;
         Assert_Truth (Check   => (C1 = C2),
                       Message => "Assignment should lead clients to be considered equal");
      end Test_Equality;

      procedure Test_Make_Client is
         S1, S2 : Servers.Server;
         C1 : Clients.Client;
         C2 : Clients.Client := C1;
      begin
         C1 := S1.Make_Client;
         Assert_Truth (Check   => (C1 /= C2),
                       Message => "Assigning a client to a server should lead to non-equal clients");

         C2 := S2.Make_Client;
         Assert_Truth (Check   => (C1 /= C2),
                       Message => "Assigning two clients to different servers should lead to non-equal clients");

         C2 := S1.Make_Client;
         Assert_Truth (Check   => (C1 = C2),
                       Message => "Assigning two clients to the same server should lead to equal servers");
      end Test_Make_Client;

      procedure Test_Pending_State is
         S : Servers.Server;
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
         S : Servers.Server;
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

            -- NOTE : Non-blocking test of Wait_Completion's blocking-ness may only be done for the Event implementation !

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
            S : Servers.Server;
            C : Clients.Client := S.Make_Client;
         begin
            C.Add_Listener (Test_Callback_Listener);
            C.Cancel;
            Test_Cancelation (S, C);
         end;

         declare
            S : Servers.Server;
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
         S : Servers.Server;
         C : Clients.Client := S.Make_Client;
      begin
         begin
            raise Custom_Error;
         exception
            when E : Custom_Error => Ada.Exceptions.Save_Occurrence (Target => Custom_Error_Occurence,
                                                                     Source => E);
         end;

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
      Test_Equality;
      Test_Make_Client;
      Test_Pending_State;
      Test_Done_State;
      Test_Canceled_State;
      Test_Error_State;
   end Run_Tests;

begin

   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Events.Servers;
