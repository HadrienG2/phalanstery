with Ada.Exceptions;
with Phalanstery.Events.Clients;
with Phalanstery.Events.Composition.And_Gates;
with Phalanstery.Events.Interfaces;
with Phalanstery.Events.Servers;
with Phalanstery.Utilities.Exceptions;
with Phalanstery.Utilities.Testing;
pragma Elaborate_All (Phalanstery.Events.Servers,
                      Phalanstery.Utilities.Testing);

package body Phalanstery.Events.Composition.Shortcuts is

   Done_Event : Events.Clients.Client;

   function When_All (Wait_List : Valid_Event_List) return Valid_Event_Client is
   begin
      -- This implementation of When_All uses AND gates if needed, but takes a performance shortcut when possible.
      if Wait_List'Length > 1 then
         declare
            Gate : And_Gates.And_Gate;
            Wait_List_Copy : Valid_Event_List := Wait_List;
         begin
            And_Gates.Add_Children (Gate, Wait_List_Copy);
            return And_Gates.Make_Client (Gate);
         end;
      elsif Wait_List'Length = 1 then
         return Wait_List (Wait_List'First);
      else
         return Done_Event;
      end if;
   end When_All;


   -- The remainder of this package is dedicated to unit tests
   procedure Run_Tests is

      use Utilities.Testing;
      use all type Events.Interfaces.Event_Status;

      procedure Test_When_None is
         Empty_List : Valid_Event_List (2 .. 1);
         E : constant Valid_Event_Client := When_All (Empty_List);
      begin
         Assert_Truth (Check   => (E.Status = Done),
                       Message => "When_All should report a done event when waiting for no event");
      end Test_When_None;

      procedure Test_When_One is
         Server : Valid_Event_Server := Servers.Make_Event;
         E : constant Valid_Event_Client := When_All ((1 => Server.Make_Client));
      begin
         Assert_Truth (Check   => (E.Status = Pending),
                       Message => "When_All should report a pending event when waiting for one event");

         Server.Mark_Done;
         Assert_Truth (Check   => (E.Status = Done),
                       Message => "The pending event reported by When_All should follow the source event");
      end Test_When_One;

      procedure Test_When_Done is
         Server1, Server2 : Valid_Event_Server := Servers.Make_Event;
         E : constant Valid_Event_Client := When_All ((Server1.Make_Client, Server2.Make_Client));
      begin
         Server1.Mark_Done;
         Assert_Truth (Check   => (E.Status = Pending),
                       Message => "The output of When_All should still be Pending when only one input event is Done");

         Server2.Mark_Done;
         Assert_Truth (Check   => (E.Status = Done),
                       Message => "The output of When_All should switch to Done when all input events are Done");
      end Test_When_Done;

      procedure Test_When_Canceled is
         Server1 : Valid_Event_Server := Servers.Make_Event;
         Server2 : constant Valid_Event_Server := Servers.Make_Event;
         E : constant Valid_Event_Client := When_All ((Server1.Make_Client, Server2.Make_Client));
      begin
         Server1.Cancel;
         Assert_Truth (Check   => (E.Status = Canceled),
                       Message => "The output of When_All should be Canceled when one input event is Canceled");
      end Test_When_Canceled;

      procedure Test_When_Error is

         Server1 : Valid_Event_Server := Servers.Make_Event;
         Server2 : constant Valid_Event_Server := Servers.Make_Event;
         E : constant Valid_Event_Client := When_All ((Server1.Make_Client, Server2.Make_Client));

         Custom_Error : exception;
         Custom_Error_Occurrence : Ada.Exceptions.Exception_Occurrence;
         Test_Error : Ada.Exceptions.Exception_Occurrence;

      begin
         Utilities.Exceptions.Make_Occurrence (What  => Custom_Error'Identity,
                                               Where => Custom_Error_Occurrence);
         Server1.Mark_Error (Custom_Error_Occurrence);
         Assert_Truth (Check   => (E.Status = Error),
                       Message => "The output of When_All should be erronerous when one input event is erronerous");

         E.Get_Error (Test_Error);
         Assert_Truth (Check   => Utilities.Exceptions.Is_Occurrence_Of (Who  => Test_Error,
                                                                         What => Child_Error'Identity),
                       Message => "Child_Error should be propagated when one input event of When_all is erronerous");
      end Test_When_Error;

   begin
      Test_When_None;
      Test_When_One;
      Test_When_Done;
      Test_When_Canceled;
      Test_When_Error;
   end Run_Tests;

begin

   -- Generate a done event for use in When_All
   declare
      E : Valid_Event_Server := Servers.Make_Event;
   begin
      E.Mark_Done;
      Done_Event := E.Make_Client;
   end;

   -- Conditionally run the unit tests on startup
   Utilities.Testing.Startup_Test (Run_Tests'Access);

end Phalanstery.Events.Composition.Shortcuts;
