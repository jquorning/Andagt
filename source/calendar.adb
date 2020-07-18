
package body Calendar is

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Date : String) return Boolean
   is
      use Ada.Calendar;
   begin
      declare
         Month : constant Month_Number :=
           Month_Number'Value (Date (Date'First + 0 .. Date'First + 1));

         Day : constant Day_Number :=
           Day_Number'Value (Date (Date'First + 3 .. Date'First + 4));

         Dummy : constant Ada.Calendar.Time := Time_Of (Year  => 2020,
                                                        Month => Month,
                                                        Day   => Day);
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
     --  Convert TS to Ada.Calendar.Time. Add on Day_Duration and convert back
     --  To Time. All happens in year with 366 days.
   is
      use Ada.Calendar;
      subtype Ada_Time is Ada.Calendar.Time;

      T1 : constant Ada_Time := Time_Of (Year   => 2020,
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
