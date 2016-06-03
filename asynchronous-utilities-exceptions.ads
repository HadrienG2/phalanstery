package body Utilities.Exceptions is

   procedure Make_Occurence (What  : Ada.Exceptions.Exception_Id;
                             Where : out Ada.Exceptions.Exception_Occurrence) is

   -- Tell whether we are dealing with a null exception occurence
   function Is_Null_Occurrence (What : Ada.Exceptions.Exception_Occurrence);

end Utilities.Exceptions;
