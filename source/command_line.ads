with Ada.Strings.Unbounded;

package Command_Line is

   package Unbounded renames Ada.Strings.Unbounded;

   subtype Ustring is Unbounded.Unbounded_String;

   function To_String (Item : Ustring) return String
     renames Unbounded.To_String;

   Andagt_File_Name : Ustring;
   Port_Number      : Natural range 0 .. 2**16 - 1;

   procedure Parse;

end Command_Line;
