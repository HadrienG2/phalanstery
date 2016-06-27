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

package body Phalanstery.Events.Clients is

   use type Implementation.Event_Reference;

   overriding function Is_Null (Who : Client) return Boolean is (Who.Ref.Is_Null);

   overriding function "=" (A, B : Client) return Boolean is (A.Ref = B.Ref);

   overriding function Status (Who : Client) return Interfaces.Event_Status is (Who.Ref.Get.Status);

   overriding procedure Get_Error (Who  : Client;
                                   What : out Ada.Exceptions.Exception_Occurrence) is
   begin
      Who.Ref.Set.Get_Error (What);
   end Get_Error;

   overriding procedure Wait_Completion (Who : Client) is
   begin
      Who.Ref.Set.Wait_Completion;
   end Wait_Completion;

   overriding procedure Add_Listener (Where : in out Client;
                                      Who   : in out Interfaces.Event_Listener_Reference'Class) is
   begin
      Where.Ref.Set.Add_Listener (Who);
   end Add_Listener;

   overriding procedure Cancel (Who : in out Client) is
   begin
      Who.Ref.Set.Cancel;
   end Cancel;

   not overriding function Make_Client (Event : Implementation.Event_Reference) return Client is ((Ref => Event));

end Phalanstery.Events.Clients;
