package body Asynchronous.Executors.Task_Queues is

   use type Ada.Containers.Count_Type;

   protected body Pending_Counter is

      procedure Add_Task is
      begin
         Count := Count + 1;
      end Add_Task;

      procedure Remove_Task is
      begin
         Count := Count - 1;
      end Remove_Task;

      function No_Pending_Task return Boolean is (Count = 0);

      entry Flush_Pending when No_Pending_Task is
      begin
         null;
      end Flush_Pending;

   end Pending_Counter;

   not overriding function Is_Empty (What : Task_Queue) return Boolean is
     ((What.Ready.Current_Use = 0) and (What.Pending.No_Pending_Task));

   overriding procedure Finalize (What : in out Task_Queue) is
   begin
      if not What.Is_Empty then
         raise Queue_Usage_Error;
      end if;
   end Finalize;

end Asynchronous.Executors.Task_Queues;
