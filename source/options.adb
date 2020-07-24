--
--  GUD BEVARE DANMARK
--

with Ustrings;

package body Options is

   subtype Ustring is Ustrings.Ustring;

   Option_File_Name : Ustring;

   ---------------
   -- File_Name --
   ---------------

   function File_Name return String
   is (Ustrings.To_String (Option_File_Name));

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name   (To : String) is
   begin
      Option_File_Name := Ustrings.From_String (To);
   end Set_File_Name;

end Options;

