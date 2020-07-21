with Ada.Calendar;

package Calendar is

   subtype Month_Number is Ada.Calendar.Month_Number;
   subtype Day_Number   is Ada.Calendar.Day_Number;

   Generic_Year : constant Ada.Calendar.Year_Number := 2020;
   --  Year with leap day.

   type Time is
      record
         Year    : Ada.Calendar.Year_Number  := Generic_Year;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Ada.Calendar.Day_Duration := 0.0;
      end record;
--   subtype Date_Of_Year is Time;

   type Datum_Number is range
     Day_Number'Last * Month_Number'First + Day_Number'First ..
     Day_Number'Last * Month_Number'Last  + Day_Number'Last;
   --  Some measure of day in year.
   --  Sparse: Contains value for 30, 31 february, 31 april, june,
   --          september,  and november. 366 + 6 = 372.

   procedure To_Date (Item    :     String;
                      Datum   : out Time;
                      Success : out Boolean);
   --  Convert Item to Date_Of_Year in Date.
   --  Item must have format "MM-DD" & ... in 366 day year
   --  or not Succss.

   function Is_Valid (Item : String) return Boolean;
   --  Date is a valid date in 366 day year.
   --  Date has format "MM-DD" & ...

   function First_Day_Of_Year return Time
   is ((Year => Generic_Year, Month => 1, Day => 1, Seconds => 0.0));
   --  First day of year

   function Last_Day_Of_Year return Time
   is ((Year => Generic_Year, Month => 12, Day => 31, Seconds => 0.0));
   --  Last day of year

   function Next (Datum : Time) return Time;
   --  Return day after TS in 366 day year.

   function Number_Of (Datum : Time) return Datum_Number
   is (Datum_Number (Day_Number'Last * Datum.Month + Datum.Day));
   --  Calculate some measure of day in year.

   function Image (Datum : Time) return String;
   --  Image of Date in format "MM-DD".

   function Date_Of_Today return Time;
   --  Todays date.

   function Date_Of_Today return String
   is (Image (Date_Of_Today));
   --  Image of todays date host local time in format "MM-DD".

end Calendar;
