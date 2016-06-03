with Ada.Exceptions;

package Asynchronous.Utilities.Exceptions is

   -- Spawn an occurence of any exception
   procedure Make_Occurrence (What  : Ada.Exceptions.Exception_Id;
                              Where : out Ada.Exceptions.Exception_Occurrence);

   -- Tell whether we are dealing with a null exception occurence
   function Is_Occurrence_Of (Who  : Ada.Exceptions.Exception_Occurrence;
                              What : Ada.Exceptions.Exception_Id) return Boolean;

   -- Tell whether we are dealing with a null exception occurence
   function Is_Null_Occurrence (Who : Ada.Exceptions.Exception_Occurrence) return Boolean;

end Asynchronous.Utilities.Exceptions;
