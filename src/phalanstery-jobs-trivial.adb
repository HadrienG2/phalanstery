-- Copyright 2016 Hadrien Grasland
--
-- This file is part of Phalanstery.
--
-- Phalanstery is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Phalanstery is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Phalanstery.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Real_Time;
with Phalanstery.Events.Clients;
with Phalanstery.Events.Contracts;
with Phalanstery.Events.Servers;
pragma Elaborate_All (Phalanstery.Events.Servers);

package body Phalanstery.Jobs.Trivial is

   subtype Valid_Event_Server is Events.Contracts.Valid_Event_Server;

   Ready_Event, Canceled_Event, Error_Event : Events.Clients.Client;

   overriding function Run (Who          : in out Null_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Who, Was_Canceled);
   begin
      return Jobs.Return_Finished;
   end Run;

   overriding function Run (Who          : in out Yielding_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
   begin
      if (Who.Counter < Who.Iterations) and not Was_Canceled then
         Who.Counter := Who.Counter + 1;
         return Jobs.Return_Yielding;
      else
         return Jobs.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who          : in out Waiting_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds);
      return Jobs.Return_Finished;
   end Run;

   overriding function Run (Who          : in out Erronerous_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds);
      raise Expected_Error;
      return Jobs.Return_Finished;  -- This line will never be reached
   end Run;

   overriding function Run (Who          : in out Ready_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds / 2);
      if not Who.Has_Waited then
         Who.Has_Waited := True;
         return Jobs.Return_Waiting (Ready_Event);
      else
         return Jobs.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who          : in out Canceled_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds);
      return Jobs.Return_Waiting (Canceled_Event);
   end Run;

   overriding function Run (Who          : in out Error_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds);
      return Jobs.Return_Waiting (Error_Event);
   end Run;

   overriding function Run (Who          : in out Custom_Wait_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds);
      return Jobs.Return_Waiting (Who.Target);
   end Run;

   overriding function Run (Who          : in out Event_Cancelation_Job;
                            Was_Canceled : Boolean) return Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Nanoseconds);
      Who.Target.Cancel;
      return Jobs.Return_Finished;
   end Run;

   procedure Busy_Wait (Nanoseconds : Natural) is
      use type Ada.Real_Time.Time, Ada.Real_Time.Time_Span;
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      End_Time : constant Ada.Real_Time.Time := Start_Time + Ada.Real_Time.Nanoseconds (Nanoseconds);
   begin
      loop
         exit when Ada.Real_Time.Clock >= End_Time;
      end loop;
   end Busy_Wait;

begin

   declare
      S : Valid_Event_Server := Events.Servers.Make_Event;
   begin
      S.Mark_Done;
      Ready_Event := S.Make_Client;
   end;

   declare
      S : Valid_Event_Server := Events.Servers.Make_Event;
   begin
      S.Cancel;
      Canceled_Event := S.Make_Client;
   end;

   declare
      S : Valid_Event_Server := Events.Servers.Make_Event;
   begin
      begin
         raise Expected_Error;
      exception
         when E : Expected_Error =>
            S.Mark_Error (E);
      end;
   end;

end Phalanstery.Jobs.Trivial;
