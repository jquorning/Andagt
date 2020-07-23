with Ada.Strings.Fixed;

with Ada.Exceptions;

package body Database is

   ----------
   -- Read --
   ----------

   procedure Read (File_Name : String)
   is
      use Ada.Text_IO;
      use Ada.Strings;

      Data_Error : exception;

      File : File_Type;
      Line_Number : Natural := 0;

      procedure Parse (Line : String);
      procedure Emit_Error (Error : String);

      -----------
      -- Parse --
      -----------

      procedure Parse (Line : String)
      is
         Pos    : constant Natural := Fixed.Index (Line, "--");
         First  : constant Natural := Line'First;
         Last   : Natural := (if Pos /= 0 then Pos - 1 else Line'Last);
         Length : Natural := Last - First + 1;
      begin
         if Length >= 1 and then Line (First) = '#' then
            Last := First - 1;
         end if;
         Length := Last - First + 1;

         --  By new end of line comments "--" and start of line comments '#'
         --  Has been ignored.

         if Length = 0 then
            return;
         end if;

         declare
            use Calendar;
            Date2   : Time;
            Success : Boolean;
            Number  : Datum_Number;
            use Fixed;
            Value_Part   : String renames Line (First + 11 .. Last);
            Comment_Part : String renames Line (Last  + 3 .. Line'Last);
            Value     : constant String := Trim (Value_Part,   Both);
            Comment   : constant String := Trim (Comment_Part, Both);
            Last_Date : Natural;
         begin
            Calendar.To_Date (Line, Last_Date, Date2, Success);
            if not Success then
               raise Data_Error with "Bad date (YYYY-MM-DD or DD-MM-YYYY)";
            end if;

            Number := Number_Of (Date2);
            if Database.Base (Number).Exists then
               raise Data_Error with "Dublicate date";
            else
               Database.Base (Number) :=
                 (Exists  => True,
                  Value   => Ustrings.From_String (Value),
                  Comment => Ustrings.From_String (Comment));
            end if;
         end;

      end Parse;

      ----------------
      -- Emit_Error --
      ----------------

      procedure Emit_Error (Error : String)
      is
         package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
         use Natural_IO;

         File : File_Type renames Standard_Error;
      begin
         Put (File, File_Name);
         Put (File, ":");
         Put (File, Line_Number, Width => 0);
         Put (File, ": ");
         Put (File, Error);
         New_Line (File);
      end Emit_Error;

   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         begin
            Line_Number := Line_Number + 1;
            Parse (Get_Line (File));
         exception
            when Occurence : Data_Error =>
               Emit_Error (Ada.Exceptions.Exception_Message (Occurence));
         end;
      end loop;
      Close (File);
   end Read;

   ------------------
   -- Find_Missing --
   ------------------

   procedure Find_Missing (File : File_Type) is
      use Calendar;
      use Ada.Text_IO;
      First : constant Time := (Year    => Generic_Year,
                                Month   => 1,
                                Day     => 1,
                                Seconds => 0.0);
      Index : Time := First;
   begin
      loop
         if not Base (Number_Of (Index)).Exists then
            Put (File, "Missing for date: ");
            Put (File, Image (Index));
            New_Line (File);
         end if;
         Index := Next (Index);   -- Depend on wrap
         exit when Index = First; -- Depend on wrap
      end loop;
   end Find_Missing;


end Database;
