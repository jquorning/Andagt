
package body Calendar is

   Year_Of_366_Days : constant Ada.Calendar.Year_Number := 2020;

   -------------
   -- To_Time --
   -------------

   procedure To_Time (Date    :     String;
                      TS      : out Time;
                      Success : out Boolean)
   is
      use Ada.Calendar;
      subtype Ada_Time is Ada.Calendar.Time;
   begin
      declare
         Month : constant Month_Number :=
           Month_Number'Value (Date (Date'First + 0 .. Date'First + 1));

         Day : constant Day_Number :=
           Day_Number'Value (Date (Date'First + 3 .. Date'First + 4));

         Ada_TS : constant Ada_Time :=
           Time_Of (Year  => Year_Of_366_Days,
                    Month => Month,
                    Day   => Day);
      begin
         Success := True;
         TS      := Time'(Month => Month,
                          Day   => Day);
      end;

   exception
      when Time_Error =>
         Success := False;

   end To_Time;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Date : String) return Boolean
   is
      Success : Boolean;
      Dummy   : Time;
      pragma Unreferenced (Dummy);
   begin
      To_Time (Date, Dummy, Success);
      return Success;
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
