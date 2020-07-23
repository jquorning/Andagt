with Ada.Text_IO;

package body Calendar is

   package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);

   -----------------
   -- Last_Day_Of --
   -----------------

   function Last_Day_Of (Month : Month_Number) return Day_Number is
   begin
      case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>  return 31;
         when 4 | 6 | 9 | 11              =>  return 30;
         when 2                           =>  return 29;
      end case;
   end Last_Day_Of;

   -------------
   -- To_Date --
   -------------

   procedure To_Date (Item    :     String;
                      Last    : out Natural;
                      Datum   : out Time;
                      Success : out Boolean)
   is
   begin
      Last    := Item'First - 1;
      Success := False;

      if Item'Length < 10 then
         return;
      end if;

      Format_ISO8601 :
      declare
         Image      : String renames Item (Item'First .. Item'First + 9);
         Year_Part  : String renames Image (1 .. 4);
         Delim_1    : String renames Image (5 .. 5);
         Month_Part : String renames Image (6 .. 7);
         Delim_2    : String renames Image (8 .. 8);
         Day_Part   : String renames Image (9 .. 10);
      begin
         if Delim_1 = "-" and Delim_2 = "-" then
            Datum.Year  := Year_Number 'Value (Year_Part);
            Datum.Month := Month_Number'Value (Month_Part);
            Datum.Day   := Day_Number  'Value (Day_Part);
            Datum.Seconds := 0.0;
            Last    := Item'First + 9;
            Success := True;
            return;
         end if;
      end Format_ISO8601;

      Format_DIN_DS :
      declare
         Image      : String renames Item (Item'First .. Item'First + 9);
         Day_Part   : String renames Image (1 .. 2);
         Delim_1    : String renames Image (3 .. 3);
         Month_Part : String renames Image (4 .. 5);
         Delim_2    : String renames Image (6 .. 6);
         Year_Part  : String renames Image (7 .. 10);
      begin
         if Delim_1 = "-" and Delim_2 = "-" then
            Datum.Day   := Day_Number  'Value (Day_Part);
            Datum.Month := Month_Number'Value (Month_Part);
            Datum.Year  := Year_Number 'Value (Year_Part);
            Datum.Seconds := 0.0;
            Last    := Item'First + 9;
            Success := True;
            return;
         end if;
      end Format_DIN_DS;

   exception
      when Constraint_Error =>
         null; -- Last and Success indicates bad format
   end To_Date;

   ----------
   -- Next --
   ----------

   function Next (Datum : Time) return Time
   is
      Datum_2 : Time := Datum;
   begin

      case Datum.Day = Last_Day_Of (Datum.Month) is

         when False =>
            Datum_2.Day := Datum.Day + 1;

         when True =>
            Datum_2.Day  := 1;
            Datum_2.Month := (if Datum.Month /= 12
                              then Datum.Month + 1
                              else 1);
      end case;
      return Datum_2;
   end Next;

   -----------
   -- Image --
   -----------

   function Image (Datum : Time) return String
   is
      Result : String (1 .. 10) := "YYYY-MM-DD";
   begin
      Natural_IO.Put (Result (1 .. 4),  Datum.Year);
      Natural_IO.Put (Result (6 .. 7),  Datum.Month);
      Natural_IO.Put (Result (9 .. 10), Datum.Day);
      if Result (1) = ' ' then Result (1) := '0'; end if;
      if Result (2) = ' ' then Result (2) := '0'; end if;
      if Result (3) = ' ' then Result (3) := '0'; end if;
      if Result (6) = ' ' then Result (6) := '0'; end if;
      if Result (9) = ' ' then Result (9) := '0'; end if;
      return Result;
   end Image;

   ---------------
   -- Image_DIN --
   ---------------

   function Image_DIN (Datum : Time) return String
   is
      Result : String (1 .. 10) := "DD-MM-YYYY";
   begin
      Natural_IO.Put (Result (1 .. 2),  Datum.Day);
      Natural_IO.Put (Result (4 .. 5),  Datum.Year);
      Natural_IO.Put (Result (7 .. 10), Datum.Month);
      if Result (1) = ' ' then Result (1) := '0'; end if;
      if Result (4) = ' ' then Result (4) := '0'; end if;
      if Result (7) = ' ' then Result (7) := '0'; end if;
      if Result (8) = ' ' then Result (8) := '0'; end if;
      if Result (9) = ' ' then Result (9) := '0'; end if;
      return Result;
   end Image_DIN;

   -----------
   -- Clock --
   -----------

   function Clock return Time
   is
      use Ada.Calendar;
      Year    : Year_Number;
      Seconds : Day_Duration;
   begin
      return Date : Time do
         Split (Clock, Year, Date.Month, Date.Day, Seconds);
      end return;
   end Clock;

end Calendar;
