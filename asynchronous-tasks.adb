package body Asynchronous.Tasks is

   function Return_Waiting (Cause : Event_Client) return Return_Value is
     ((State => Waiting, Wait_List_Length => 1, Wait_List => (1 => Cause)));

   function Return_Waiting (Cause : Event_Wait_List) return Return_Value is
     ((State => Waiting, Wait_List_Length => Cause'Length, Wait_List => Cause));

   function Status (What : Return_Value) return Return_Status is
     (What.State);

   function Wait_List (What : Return_Value) return Event_Wait_List is
     (What.Wait_List);

end Asynchronous.Tasks;
