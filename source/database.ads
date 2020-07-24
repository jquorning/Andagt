--
--  GUD BEVARE DANMARK
--

with Ada.Text_IO;

with Ustrings;
with Calendar;

package Database is

   subtype Ustring   is Ustrings.Ustring;
   subtype File_Type is Ada.Text_IO.File_Type;

   Null_Ustring : constant Ustring := Ustrings.Null_Ustring;

   type Data_Point is
      record
         Exists  : Boolean;
         Value   : Ustring;
         Comment : Ustring;
      end record;
   --  What to store about a day.

   Null_Data_Point : constant Data_Point := (Exists  => False,
                                             Value   => Null_Ustring,
                                             Comment => Null_Ustring);

   Base : array (Calendar.Datum_Number) of Data_Point :=
     (others => Null_Data_Point);

   procedure Read (File_Name : String);
   --  Read andagt file names File_Name.

   procedure Find_Missing (File : File_Type);
   --  Find and print out missing dates in Base.

end Database;
