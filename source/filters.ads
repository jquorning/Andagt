--
--  GUD BEVARE DANMARK
--

with Calendar;

package Filters is

   subtype Time is Calendar.Time;

   function Day_Of (Datum : Time) return String;

   function Month_Name_En (Datum : Time) return String;

   procedure Register;

end Filters;
