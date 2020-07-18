with Ada.Command_Line;

package body Command_Line is

   Default_Port_Number : constant := 80;

   function To_Ustring (Item : String) return Ustring
     renames Unbounded.To_Unbounded_String;

   -----------
   -- Parse --
   -----------

   procedure Parse is
      Arguments : constant Natural := Ada.Command_Line.Argument_Count;
   begin
      if Arguments > 2 then
         raise Constraint_Error with "More than two command line arguments";
      elsif Arguments = 2 then
         Andagt_File_Name := To_Ustring    (Ada.Command_Line.Argument (1));
         Port_Number      := Natural'Value (Ada.Command_Line.Argument (2));
      elsif Arguments = 1 then
         Andagt_File_Name := To_Ustring (Ada.Command_Line.Argument (1));
         Port_Number      := Default_Port_Number;
      else
         raise Constraint_Error with "Require one or tow command line arguemnts";
      end if;
   end Parse;

end Command_Line;
