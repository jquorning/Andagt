with Ada.Calendar;

package Calendar is

   subtype Year_Number  is Ada.Calendar.Year_Number;
   subtype Month_Number is Ada.Calendar.Month_Number;
   subtype Day_Number   is Ada.Calendar.Day_Number;
   subtype Day_Duration is Ada.Calendar.Day_Duration;

   Generic_Year : constant Ada.Calendar.Year_Number := 2020;
   --  Year with leap day.

   type Time is
      record
         Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration;
      end record;

   type Datum_Number is range
     Day_Number'Last * Month_Number'First + Day_Number'First ..
     Day_Number'Last * Month_Number'Last  + Day_Number'Last;
   --  Some measure of day in year.
   --  Sparse: Contains value for 30, 31 february, 31 april, june,
   --          september,  and november. 366 + 6 = 372.

   procedure To_Date (Item    :     String;
                      Last    : out Natural;
                      Datum   : out Time;
                      Success : out Boolean);
   --  Convert Item to Time in Datum. Item must have format "YYYY-MM-DD" or
   --  "DD-MM-YYYY" or not Success. Last is position of last character in parsing.
   --  Check validity of date but not leap year. This means that "1999-02-29" is
   --  Success but "1999-02-30" is not.

   function Next (Datum : Time) return Time;
   --  Return day after TS in 366 day year.

   function Number_Of (Datum : Time) return Datum_Number
   is (Datum_Number (Day_Number'Last * Datum.Month + Datum.Day));
   --  Calculate some measure of day in year.

   function Image (Datum : Time) return String;
   --  Image of Datum in ISO format "YYYY-MM-DD".

   function Image_DIN (Datum : Time) return String;
   --  Image of Datum in DIN format "DD-MM-YYYY".

   function Clock return Time;
   --  Todays datum.

   function Last_Day_Of (Month : Month_Number) return Day_Number;
   --  Return last Day_Number of Month.

end Calendar;
