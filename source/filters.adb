--
--  GUD BEVARE DANMARK
--

with Ada.Strings.Fixed;

with AWS.Templates;
with AWS.URL;

package body Filters is

   subtype Filter_Context is AWS.Templates.Filter_Context;

   function Safe_URL
     (Value   : in String;
      Context : in Filter_Context) return String;
   --  Tanslate Value to safe URL (Space - > %20)

   ------------
   -- Day_Of --
   ------------

   function Day_Of (Datum : Time) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Calendar.Day_Number'Image (Datum.Day), Both);
   end Day_Of;

   -------------------
   -- Month_Name_En --
   -------------------

   function Month_Name_En (Datum : Time) return String
   is (Month_Name_En (Datum.Month));

   function Month_Name_En (Month : Month_Number) return String is
   begin
      case Month is
         when 1 =>   return "January";
         when 2  =>  return "February";
         when 3  =>  return "March";
         when 4  =>  return "April";
         when 5  =>  return "May";
         when 6  =>  return "June";
         when 7  =>  return "July";
         when 8  =>  return "August";
         when 9  =>  return "September";
         when 10 =>  return "October";
         when 11 =>  return "November";
         when 12 =>  return "December";
      end case;
   end Month_Name_En;

   -------------------
   -- Month_Name_Da --
   -------------------

   function Month_Name_Da (Datum : Time) return String
   is (Month_Name_En (Datum.Month));

   function Month_Name_Da (Month : Month_Number) return String is
   begin
      case Month is
         when 1 =>   return "Januar";
         when 2  =>  return "Februar";
         when 3  =>  return "Marts";
         when 4  =>  return "April";
         when 5  =>  return "Maj";
         when 6  =>  return "Juni";
         when 7  =>  return "Juli";
         when 8  =>  return "August";
         when 9  =>  return "September";
         when 10 =>  return "Oktober";
         when 11 =>  return "November";
         when 12 =>  return "December";
      end case;
   end Month_Name_Da;

   --------------
   -- Safe_URL --
   --------------

   function Safe_URL
     (Value   : in String;
      Context : in Filter_Context) return String
   is
      pragma Unreferenced (Context);
   begin
      return AWS.URL.Encode (Value);
   end Safe_URL;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      AWS.Templates.Register_Filter ("WEB_SAFE_URL", Safe_URL'Access);
   end Register;

end Filters;
