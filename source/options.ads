package Options is

   Default_Port_Number : constant := 80;

   function File_Name  return String;
   function Port_Image return String;
   function Port       return Natural;

   procedure Set_File_Name   (To : String);
   procedure Set_Port_Number (To : String);

end Options;

