package body Events.Clients is

   use type Implementation.Event_Reference;

   overriding function Is_Null (Who : Client) return Boolean is (Who.Ref.Is_Null);

   overriding function "=" (A, B : Client) return Boolean is (A.Ref = B.Ref);

   overriding function Status (Who : Client) return Event_Status is (Who.Ref.Get.Status);

   overriding procedure Get_Error (Who  : Client;
                                   What : out Ada.Exceptions.Exception_Occurrence) is
   begin
      Who.Ref.Get.Get_Error (What);
   end Get_Error;

   overriding procedure Wait_Completion (Who          : Client;
                                         Final_Status : out Finished_Event_Status) is
   begin
      Who.Ref.Get.Wait_Completion (Final_Status);
   end Wait_Completion;

   overriding procedure Add_Listener (Where : in out Client;
                                      Who   : in out Event_Listener_Reference'Class) is
   begin
      Where.Ref.Get.Add_Listener (Who);
   end Add_Listener;

   overriding procedure Cancel (Who : in out Client) is
   begin
      Who.Ref.Get.Cancel;
   end Cancel;

   not overriding function Make_Client (Event : Implementation.Event_Reference) return Client is ((Ref => Event));

end Events.Clients;
