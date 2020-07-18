with Ada.Text_IO;

with Command_Line;
with Database;

procedure Andagt is
   use Ada.Text_IO;
   use Command_Line;
begin
   Put_Line ("Andagt version 0.0.0");
   Command_Line.Parse;
   Put ("Andagt file: ");  Put (File_Name);             New_Line;
   Put ("Port number:");   Put (Natural'Image (Port));  New_Line;
   Database.Read (File_Name);
end Andagt;
