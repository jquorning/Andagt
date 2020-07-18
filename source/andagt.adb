with Ada.Text_IO;

with Command_Line;

procedure Andagt is
   use Ada.Text_IO;
   use Command_Line;
begin
   Put_Line ("Andagt version 0.0.0");
   Command_Line.Parse;
   Put ("Andagt file: "); Put (To_String (Andagt_File_Name));  New_Line;
   Put ("Port number:");  Put (Natural'Image (Port_Number));   New_Line;
end Andagt;
