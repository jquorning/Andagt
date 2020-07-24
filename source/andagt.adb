--
--  GUD BEVARE DANMARK
--

with Ada.Text_IO;

with Command_Line;
with Options;
with Database;
with HTTP_Server;

procedure Andagt is
   use Ada.Text_IO;
begin
   Command_Line.Parse;
   Database.Read (Options.File_Name);
   Database.Find_Missing (Standard_Error);
   HTTP_Server.Startup;
   delay 1000.0;
   HTTP_Server.Shutdown;
end Andagt;
