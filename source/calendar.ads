with Ada.Calendar;

package Calendar is

   subtype Month_Number is Ada.Calendar.Month_Number;
   subtype Day_Number   is Ada.Calendar.Day_Number;

   type Time is
      record
         Month : Month_Number;
         Day   : Day_Number;
      end record;

   type Day_Measure is range
     32 * Month_Number'First + Day_Number'First ..
     32 * Month_Number'Last  + Day_Number'Last;
   --  Some measure of day in year.

   function Is_Valid (Date : String) return Boolean;
   --  Date is a valid date in 366 day year.
   --  Date has format "MM-DD" & ...

   function First return Time
   is (Time'(Month => 01, Day => 01));
   --  First day of year

   function Last  return Time
   is (Time'(Month => 12, Day => 31));
   --  Last day of year

   function Next (TS : Time) return Time;
   --  Return day after TS in 366 day year.

   function "=" (Left, Right : Time) return Boolean
   is (Left.Month = Right.Month and
         Left.Day = Right.Day);
   --  Compare days.

   function To_Day_Number (TS : Time) return Day_Measure
   is (Day_Measure (32 * TS.Month + TS.Day));
   --  Calculate some measure of day in year.

end Calendar;
