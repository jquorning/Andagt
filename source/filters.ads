--
--  GUD BEVARE DANMARK
--

with Calendar;

package Filters is

   subtype Time         is Calendar.Time;
   subtype Month_Number is Calendar.Month_Number;

   function Day_Of (Datum : Time) return String;

   function Month_Name_En (Datum : Time) return String;
   function Month_Name_En (Month : Month_Number) return String;
   function Month_Name_Da (Datum : Time;
                           Full  : Boolean) return String;
   function Month_Name_Da (Month : Month_Number;
                           Full  : Boolean) return String;

   procedure Register;

end Filters;
