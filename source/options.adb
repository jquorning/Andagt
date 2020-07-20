with Ustrings;

package body Options is

   subtype Ustring is Ustrings.Ustring;

   Option_File_Name   : Ustring;
   Option_Port_Number : Natural range 0 .. 2**16 - 1;


   ---------------
   -- File_Name --
   ---------------

   function File_Name return String
   is (Ustrings.To_String (Option_File_Name));

   ----------
   -- Port --
   ----------

   function Port return Natural
   is (Option_Port_Number);

   ----------------
   -- Port_Image --
   ----------------

   function Port_Image return String is
   begin
      return Port'Image;
   end Port_Image;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name   (To : String) is
   begin
      Option_File_Name := Ustrings.To_Ustring (To);
   end Set_File_Name;

   ---------------------
   -- Set_Port_Number --
   ---------------------

   procedure Set_Port_Number (To : String) is
   begin
      Option_Port_Number := Natural'Value (To);
   end Set_Port_Number;

end Options;

