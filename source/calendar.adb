with Ada.Text_IO;

package body Calendar is

   function Last_Day_Of (Month : Month_Number) return Day_Number;
   --  Return last Day_Number of Month.

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
                      Datum   : out Time;
                      Success : out Boolean)
   is
      Month_Part : String renames Item (Item'First + 0 .. Item'First + 1);
      Day_Part   : String renames Item (Item'First + 3 .. Item'First + 4);
   begin
      Success      := False;   -- Pessimistic
      Datum.Month   := Month_Number'Value (Month_Part);
      Datum.Day     := Day_Number  'Value (Day_Part);
      Datum.Year    := Generic_Year;
      Datum.Seconds := 0.0;
      Success      := True;    -- Optimist again
   exception
      when Constraint_Error =>
         Success   := False;   -- Confirmed bad
   end To_Date;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Item : String) return Boolean
   is
      Success : Boolean;
      Dummy   : Time;
      pragma Unreferenced (Dummy);
   begin
      To_Date (Item, Dummy, Success);
      return Success;
   end Is_Valid;

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
      Result : String (1 .. 5) := "MM-DD";
   begin
      Natural_IO.Put (Result (1 .. 2), Datum.Month);
      Natural_IO.Put (Result (4 .. 5), Datum.Day);
      if Result (1) = ' ' then Result (1) := '0'; end if;
      if Result (4) = ' ' then Result (4) := '0'; end if;
      return Result;
   end Image;

   -------------------
   -- Date_Of_Today --
   -------------------

   function Date_Of_Today return Time
   is
      use Ada.Calendar;
      Year    : Year_Number;
      Seconds : Day_Duration;
   begin
      return Date : Time do
         Split (Clock, Year, Date.Month, Date.Day, Seconds);
      end return;
   end Date_Of_Today;

end Calendar;
