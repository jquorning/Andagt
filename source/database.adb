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

         --  By not end of line comments "--" and start of line comments '#'
         --  Has been ignored.

         if Length = 0 then
            return;
         end if;

         --  By not the first six characters of Line is expected to contain
         --  date ending with space "MM-DD ".

         if Length < 5 then
            raise Data_Error with "Line to short for date (MM-DD)";
         end if;

         declare
            Date : String renames Line (First .. First + 5);
         begin
            --  Month part
            if
              Date (Date'First + 0) not in '0' .. '1' or
              Date (Date'First + 1) not in '0' .. '9'
            then
               raise Data_Error with "Bad month part (MM-DD)";
            end if;

            --  Separator
            if Date (Date'First + 2) /= '-' then
               raise Data_Error with "Bad data separator (MM-DD)";
            end if;

            --  Day part
            if
              Date (Date'First + 3) not in '0' .. '3' or
              Date (Date'First + 4) not in '0' .. '9'
            then
               raise Data_Error with "Bad day part (MM-DD)";
            end if;

            --  Check if Date is a valid date
            if not Calendar.Is_Valid (Date) then
               raise Data_Error with "Bad date";
            end if;

            declare
               use Calendar;
               Date2   : Time;
               Success : Boolean;
               Number  : Date_Number;
               use Fixed;
               Value_Part   : String renames Line (First + 5 .. Last);
               Comment_Part : String renames Line (Last  + 3 .. Line'Last);
               Value   : constant String := Trim (Value_Part,   Both);
               Comment : constant String := Trim (Comment_Part, Both);
            begin
               Calendar.To_Date (Date, Date2, Success);
               Number := To_Date_Number (Date2);
               if Database.Base (Number).Exists then
                  raise Data_Error with "Dublicate date";
               else
                  Database.Base (Number) :=
                    (Exists  => True,
                     Value   => Ustrings.From_String (Value),
                     Comment => Ustrings.From_String (Comment));
               end if;
            end;

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
      Index : Time := First_Day_Of_Year;
   begin
      while Index /= Last_Day_Of_Year loop
         if not Base (To_Date_Number (Index)).Exists then
            Put (File, "Missing for date: ");
            Put (File, Image (Index));
            New_Line (File);
         end if;
         Index := Next (Index);
      end loop;
   end Find_Missing;


end Database;
