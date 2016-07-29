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
with Phalanstery.Outcomes.Clients;
with Phalanstery.Outcomes.Servers;
pragma Elaborate_All (Phalanstery.Outcomes.Servers);

package body Phalanstery.Examples.Trivial_Jobs is

   subtype Valid_Outcome_Server is Outcomes.Contracts.Valid_Outcome_Server;

   Ready_Outcome, Canceled_Outcome, Error_Outcome : Outcomes.Clients.Client;

   overriding function Run (Who          : in out Null_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Who, Was_Canceled);
   begin
      return Asynchronous_Jobs.Return_Finished;
   end Run;

   overriding function Run (Who          : in out Self_Canceling_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Who, Was_Canceled);
   begin
      return Asynchronous_Jobs.Return_Canceled;
   end Run;

   overriding function Run (Who          : in out Yielding_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
   begin
      if (Who.Counter < Who.Iterations) and not Was_Canceled then
         Who.Counter := Who.Counter + 1;
         return Asynchronous_Jobs.Return_Yielding;
      else
         return Asynchronous_Jobs.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who          : in out Waiting_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds);
      return Asynchronous_Jobs.Return_Finished;
   end Run;

   overriding function Run (Who          : in out Erronerous_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds);
      raise Expected_Error;
      return Asynchronous_Jobs.Return_Finished;  -- This line will never be reached
   end Run;

   overriding function Run (Who          : in out Ready_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds / 2);
      if not Who.Has_Waited then
         Who.Has_Waited := True;
         return Asynchronous_Jobs.Return_Waiting (Ready_Outcome);
      else
         return Asynchronous_Jobs.Return_Finished;
      end if;
   end Run;

   overriding function Run (Who          : in out Canceled_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds);
      return Asynchronous_Jobs.Return_Waiting (Canceled_Outcome);
   end Run;

   overriding function Run (Who          : in out Error_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds);
      return Asynchronous_Jobs.Return_Waiting (Error_Outcome);
   end Run;

   overriding function Run (Who          : in out Custom_Wait_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds);
      return Asynchronous_Jobs.Return_Waiting (Who.Target);
   end Run;

   overriding function Run (Who          : in out Cancelation_Job;
                            Was_Canceled : Boolean) return Asynchronous_Jobs.Return_Value is
      pragma Unreferenced (Was_Canceled);
   begin
      Busy_Wait (Who.Waiting_Microseconds);
      Who.Target.Cancel;
      return Asynchronous_Jobs.Return_Finished;
   end Run;

   procedure Busy_Wait (Microseconds : Natural) is
      use type Ada.Real_Time.Time, Ada.Real_Time.Time_Span;
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      End_Time : constant Ada.Real_Time.Time := Start_Time + Ada.Real_Time.Microseconds (Microseconds);
   begin
      loop
         exit when Ada.Real_Time.Clock >= End_Time;
      end loop;
   end Busy_Wait;

begin

   declare
      S : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
   begin
      S.Mark_Done;
      Ready_Outcome := S.Make_Client;
   end;

   declare
      S : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
   begin
      S.Cancel;
      Canceled_Outcome := S.Make_Client;
   end;

   declare
      S : Valid_Outcome_Server := Outcomes.Servers.Make_Outcome;
   begin
      begin
         raise Expected_Error;
      exception
         when E : Expected_Error =>
            S.Mark_Error (E);
      end;
   end;

end Phalanstery.Examples.Trivial_Jobs;
