
package body Calendar is

   Year_Of_366_Days : constant Ada.Calendar.Year_Number := 2020;

   -------------
   -- To_Time --
   -------------

   function To_Time (Date : String) return Time
   is
      use Ada.Calendar;

      Month : constant Month_Number :=
        Month_Number'Value (Date (Date'First + 0 .. Date'First + 1));

      Day : constant Day_Number :=
        Day_Number'Value (Date (Date'First + 3 .. Date'First + 4));

      TS : constant Ada.Calendar.Time :=
        Time_Of (Year  => Year_Of_366_Days,
                 Month => Month,
                 Day   => Day);
   begin
      return
        Time'(Month => Month,
              Day   => Day);
   end To_Time;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Date : String) return Boolean
   is
   begin
      declare
         Dummy : constant Time := To_Time (Date);
         pragma Unreferenced (Dummy);
      begin
         null;
      exception
         when others =>
            return False;
      end;
      return True;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   function Next (TS : Calendar.Time) return Calendar.Time
     --  Convert TS to Ada.Calendar.Time. Add on Day_Duration and
     --  convert back To Time. All happens in year with 366 days.
   is
      use Ada.Calendar;
      subtype Ada_Time is Ada.Calendar.Time;

      T1 : constant Ada_Time := Time_Of (Year   => Year_Of_366_Days,
                                         Month => TS.Month,
                                         Day   => TS.Day);
      T2 : constant Ada_Time := T1 + Day_Duration'Last;
      Year    : Year_Number;
      Seconds : Day_Duration;
   begin
      return Result : Time
      do
         Split (Date    => T2,
                Year    => Year,
                Month   => Result.Month,
                Day     => Result.Day,
                Seconds => Seconds);
      end return;
   end Next;

end Calendar;
