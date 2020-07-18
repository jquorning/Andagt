package Command_Line is

   procedure Parse;
   --  Parse the command line;

   function File_Name return String;
   function Port return Natural;

end Command_Line;
