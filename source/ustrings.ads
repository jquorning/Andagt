with Ada.Strings.Unbounded;

package Ustrings is

   package Unbounded renames Ada.Strings.Unbounded;

   subtype Ustring is Unbounded.Unbounded_String;

   Null_Ustring : constant Ustring := Unbounded.Null_Unbounded_String;

   function To_Ustring (Item : String) return Ustring
     renames Unbounded.To_Unbounded_String;

   function To_String (Item : Ustring) return String
     renames Unbounded.To_String;

end Ustrings;
