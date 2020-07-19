with Ada.Calendar;

package Calendar is

   subtype Month_Number is Ada.Calendar.Month_Number;
   subtype Day_Number   is Ada.Calendar.Day_Number;

   type Date_Of_Year is
      record
         Month : Month_Number;
         Day   : Day_Number;
      end record;

   type Date_Number is range
     32 * Month_Number'First + Day_Number'First ..
     32 * Month_Number'Last  + Day_Number'Last;
   --  Some measure of day in year.

   procedure To_Date (Item    :     String;
                      Date    : out Date_Of_Year;
                      Success : out Boolean);
   --  Convert Item to Date_Of_Year in Date.
   --  Item must have format "MM-DD" & ... in 366 day year
   --  or not Succss.

   function Is_Valid (Item : String) return Boolean;
   --  Date is a valid date in 366 day year.
   --  Date has format "MM-DD" & ...

   function First return Date_Of_Year
   is (Date_Of_Year'(Month => 01, Day => 01));
   --  First day of year

   function Last  return Date_Of_Year
   is (Date_Of_Year'(Month => 12, Day => 31));
   --  Last day of year

   function Next (Date : Date_Of_Year) return Date_Of_Year;
   --  Return day after TS in 366 day year.

   function "=" (Left, Right : Date_Of_Year) return Boolean
   is (Left.Month = Right.Month and
         Left.Day = Right.Day);
   --  Compare days.

   function To_Date_Number (Date : Date_Of_Year) return Date_Number
   is (Date_Number (32 * Date.Month + Date.Day));
   --  Calculate some measure of day in year.

end Calendar;
