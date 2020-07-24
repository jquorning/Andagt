with Ada.Command_Line;

with Options;

package body Command_Line is

   -----------
   -- Parse --
   -----------

   procedure Parse
   is
      use Ada.Command_Line;

      Arguments : constant Natural := Argument_Count;
   begin
      if Arguments > 1 then
         raise Constraint_Error
           with "More than two command line arguments";
      elsif Arguments = 1 then
         Options.Set_File_Name (Argument (1));
      else
         raise Constraint_Error
           with "Require one or tow command line arguemnts";
      end if;
   end Parse;

end Command_Line;
