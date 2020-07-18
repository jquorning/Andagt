with Ada.Command_Line;
with Ada.Strings.Unbounded;

package body Command_Line is

   Default_Port_Number : constant := 80;

   package Unbounded renames Ada.Strings.Unbounded;

   subtype Ustring is Unbounded.Unbounded_String;

   function To_Ustring (Item : String) return Ustring
     renames Unbounded.To_Unbounded_String;

   function To_String (Item : Ustring) return String
     renames Unbounded.To_String;

   Option_File_Name   : Ustring;
   Option_Port_Number : Natural range 0 .. 2**16 - 1;

   -----------
   -- Parse --
   -----------

   procedure Parse
   is
      use Ada.Command_Line;

      Arguments : constant Natural := Argument_Count;
   begin
      if Arguments > 2 then
         raise Constraint_Error
           with "More than two command line arguments";
      elsif Arguments = 2 then
         Option_File_Name   := To_Ustring    (Argument (1));
         Option_Port_Number := Natural'Value (Argument (2));
      elsif Arguments = 1 then
         Option_File_Name   := To_Ustring (Argument (1));
         Option_Port_Number := Default_Port_Number;
      else
         raise Constraint_Error
           with "Require one or tow command line arguemnts";
      end if;
   end Parse;

   ---------------
   -- File_Name --
   ---------------

   function File_Name return String
   is (To_String (Option_File_Name));

   ----------
   -- Port --
   ----------

   function Port return Natural
   is (Option_Port_Number);

end Command_Line;
