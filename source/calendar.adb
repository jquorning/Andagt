with Ada.Text_IO;

package body Calendar is

   function Last_Day (Month : Month_Number) return Day_Number;
   function Last_Day (Month : Month_Number) return Day_Number is
   begin
      case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>  return 31;
         when 4 | 6 | 9 | 11              =>  return 30;
         when 2                           =>  return 29;
      end case;
   end Last_Day;

   -------------
   -- To_Date --
   -------------

   procedure To_Date (Item    :     String;
                      Date    : out Date_Of_Year;
                      Success : out Boolean)
   is
      Month_Part : String renames Item (Item'First + 0 .. Item'First + 1);
      Day_Part   : String renames Item (Item'First + 3 .. Item'First + 4);
   begin
      Date.Month := Month_Number'Value (Month_Part);
      Date.Day   := Day_Number'Value (Day_Part);
      Success    := Date.Day <= Last_Day (Date.Month);
   exception
      when Constraint_Error =>
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
   is
      Date_2 : Date_Of_Year;
   begin
      case Date.Day = Last_Day (Date.Month) is

         when False =>
            Date_2.Day   := Date.Day + 1;
            Date_2.Month := Date.Month;

         when True =>
            Date_2.Day   := 1;
            Date_2.Month := (if Date.Month /= 12
                               then Date.Month + 1
                               else 1);
      end case;
      return Date_2;
   end Next;

   -----------
   -- Image --
   -----------

   function Image (Date : Date_Of_Year) return String
   is
      package Natural_IO is new Ada.Text_IO.Integer_IO (Natural);
      Result : String (1 .. 5);
   begin
      Natural_IO.Put (Result (1 .. 2), Date.Month);
      Natural_IO.Put (Result (4 .. 5), Date.Day);
      Result (3) := '-';
      if Result (1) = ' ' then Result (1) := '0'; end if;
      if Result (4) = ' ' then Result (4) := '0'; end if;
      return Result;
   end Image;

end Calendar;
