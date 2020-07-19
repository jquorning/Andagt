--  with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Calendar;

package Database is

   subtype Ustring is Ada.Strings.Unbounded.Unbounded_String;

   Null_Ustring : constant Ustring :=
     Ada.Strings.Unbounded.Null_Unbounded_String;

   type Data_Point is
      record
         Exists : Boolean;
         Title  : Ustring;
         URL    : Ustring;
      end record;
   --  What to store about a day.

   Null_Data_Point : constant Data_Point := (Exists => False,
                                             Title  => Null_Ustring,
                                             URL    => Null_Ustring);

   Base : array (Calendar.Date_Number) of Data_Point :=
     (others => Null_Data_Point);

   procedure Read (File_Name : String);
   --  Read andagt file names File_Name.

end Database;
