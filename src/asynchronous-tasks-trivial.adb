with Asynchronous.Events.Servers;

package body Asynchronous.Tasks.Trivial is

   Ready_Event, Canceled_Event, Error_Event : Event_Client;

   overriding function Run (Who : in out Null_Task) return Tasks.Return_Value is (Tasks.Return_Finished);

   overriding function Run (Who : in out Yielding_Task) return Tasks.Return_Value is
   begin
      if Who.Counter < Who.Iterations then
         Who.Counter := Who.Counter + 1;
         return Tasks.Return_Yielding;
      else
         return Tasks.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who : in out Ready_Wait_Task) return Tasks.Return_Value is
   begin
      if not Who.Has_Waited then
         Who.Has_Waited := True;
         return Tasks.Return_Waiting (Ready_Event);
      else
         return Tasks.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who : in out Canceled_Wait_Task) return Tasks.Return_Value is
     (Tasks.Return_Waiting (Canceled_Event));

   overriding function Run (Who : in out Error_Wait_Task) return Tasks.Return_Value is
     (Tasks.Return_Waiting (Error_Event));

   overriding function Run (Who : in out Custom_Wait_Task) return Tasks.Return_Value is
     (Tasks.Return_Waiting (Who.Target));

   overriding function Run (Who : in out Event_Cancelation_Task) return Tasks.Return_Value is
   begin
      Who.Target.Cancel;
      return Tasks.Return_Finished;
   end Run;

begin

   declare
      S : Asynchronous.Events.Servers.Server := Asynchronous.Events.Servers.Make_Event;
   begin
      S.Mark_Done;
      Ready_Event := S.Make_Client;
   end;

   declare
      S : Asynchronous.Events.Servers.Server := Asynchronous.Events.Servers.Make_Event;
   begin
      S.Cancel;
      Canceled_Event := S.Make_Client;
   end;

   declare
      S : Asynchronous.Events.Servers.Server := Asynchronous.Events.Servers.Make_Event;
   begin
      begin
         raise Expected_Error;
      exception
         when E : Expected_Error =>
            S.Mark_Error (E);
      end;
   end;

end Asynchronous.Tasks.Trivial;
