
package body Calendar is

   Year_Of_366_Days : constant Ada.Calendar.Year_Number := 2020;

   -------------
   -- To_Time --
   -------------

   procedure To_Date (Item    :     String;
                      Date    : out Date_Of_Year;
                      Success : out Boolean)
   is
      use all type Ada.Calendar.Time;
      subtype Ada_Time is Ada.Calendar.Time;
   begin
      declare
         Month : constant Month_Number :=
           Month_Number'Value (Item (Item'First + 0 .. Item'First + 1));

         Day : constant Day_Number :=
           Day_Number'Value (Item (Item'First + 3 .. Item'First + 4));

         Ada_TS : constant Ada_Time :=
           Time_Of (Year  => Year_Of_366_Days,
                    Month => Month,
                    Day   => Day);
      begin
         Success := True;
         Date    := Date_Of_Year'(Month => Month,
                                  Day   => Day);
      end;

   exception
      when Ada.Calendar.Time_Error =>
         Success := False;

   end To_Date;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Item : String) return Boolean
   is
      Success : Boolean;
      Dummy   : Date_Of_Year;
      pragma Unreferenced (Dummy);
   begin
      To_Date (Item, Dummy, Success);
      return Success;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   function Next (Date : Date_Of_Year) return Date_Of_Year
     --  Convert Date to Ada.Calendar.Time. Add on Day_Duration and
     --  convert back To Date_Of_Year. All happens in year with 366 days.
   is
      use Ada.Calendar;
      subtype Ada_Time is Ada.Calendar.Time;

      T1 : constant Ada_Time := Time_Of (Year   => Year_Of_366_Days,
                                         Month => Date.Month,
                                         Day   => Date.Day);
      T2 : constant Ada_Time := T1 + Day_Duration'Last;
      Year    : Year_Number;
      Seconds : Day_Duration;
   begin
      return Result : Date_Of_Year
      do
         Split (Date    => T2,
                Year    => Year,
                Month   => Result.Month,
                Day     => Result.Day,
                Seconds => Seconds);
      end return;
   end Next;

end Calendar;
