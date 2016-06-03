with Ada.Exceptions;

package Asynchronous.Utilities.Exceptions is

   -- Spawn an occurence of any exception
   procedure Make_Occurence (What  : Ada.Exceptions.Exception_Id;
                             Where : out Ada.Exceptions.Exception_Occurrence);

   -- Tell whether we are dealing with a null exception occurence
   function Is_Null_Occurrence (What : Ada.Exceptions.Exception_Occurrence);

end Asynchronous.Utilities.Exceptions;
