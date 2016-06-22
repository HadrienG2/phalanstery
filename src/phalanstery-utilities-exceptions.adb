package body Phalanstery.Utilities.Exceptions is

   use type Ada.Exceptions.Exception_Id;

   procedure Make_Occurrence (What  : Ada.Exceptions.Exception_Id;
                              Where : out Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Exceptions.Raise_Exception (What);
   exception
      when E : others =>
         Ada.Exceptions.Save_Occurrence (Target => Where,
                                         Source => E);
   end Make_Occurrence;

   function Is_Occurrence_Of (Who  : Ada.Exceptions.Exception_Occurrence;
                              What : Ada.Exceptions.Exception_Id) return Boolean is
     (Ada.Exceptions.Exception_Identity (Who) = What);

   function Is_Null_Occurrence (Who : Ada.Exceptions.Exception_Occurrence) return Boolean is
     (Is_Occurrence_Of (Who, Ada.Exceptions.Null_Id));

end Phalanstery.Utilities.Exceptions;
