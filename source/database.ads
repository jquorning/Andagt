with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Calendar;

package Database is

   subtype Ustring is Ada.Strings.Unbounded.Unbounded_String;

   procedure Read (File_Name : String);
   --  Read andagt file names File_Name.

   type Data_Point is
      record
         Title : Ustring;
         URL   : Ustring;
      end record;

   package Data_Vectors
   is new Ada.Containers.Vectors
     (Index_Type   => Calendar.Day_Number,
      Element_Type => Data_Point);

   Base : Data_Vectors.Vector;

end Database;
