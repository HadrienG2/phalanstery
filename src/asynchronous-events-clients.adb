package body Asynchronous.Events.Clients is

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

end Asynchronous.Events.Clients;
