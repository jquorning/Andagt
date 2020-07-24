--
--  GUD BEVARE DANMARK
--

with Ada.Strings.Unbounded;

with AWS.MIME;
with AWS.Templates;
with AWS.Parameters;

with Ustrings;
with Options;
with Calendar;
with Database;
with Filters;

package body WWW_Manager is

   WWW_Base : constant String := "www/";

   subtype Ustring is Ustrings.Ustring;
   subtype Time    is Calendar.Time;

   function Dispatch_TOC   (Request : Status_Data) return Response_Data;
   function Dispatch_Daily (Request : Status_Data) return Response_Data;

   function Build_TOC return Ustring;

   Translations : AWS.Templates.Translate_Set;

   -------------------------
   -- Dispatch WWW_Action --
   -------------------------

   overriding function Dispatch
     (Handler : in WWW_Action;
      Request : in Status_Data) return Response_Data
   is
      use AWS;
      URI : constant String := Status.URI (Request);
   begin
      AWS.Templates.Insert (Translations, AWS.Templates.Assoc ("ANDAGTSBOG", Options.File_Name));

      if    URI = "/"       then  return Dispatch_Daily (Request);
      elsif URI = "/daglig" then  return Dispatch_Daily (Request);
      elsif URI = "/daily"  then  return Dispatch_Daily (Request);
      else                        return Dispatch_TOC (Request);
      end if;
   end Dispatch;

   ---------------
   -- Build_TOC --
   ---------------

   function HREF_Image (Datum : Time) return String;

   function HREF_Image (Datum : Time) return String is
      Day_Image  : constant String := Calendar.Image (Datum);
      URL_Image  : constant String := "http://localhost:8080/?day=" & Day_Image;
      Quoted     : constant String := """" & URL_Image & """";
   begin
      return Quoted;
   end HREF_Image;


   function Build_TOC return Ustring is
      use Ada.Strings.Unbounded;
      use Calendar;
      New_Line : Character renames ASCII.LF;
      Buffer   : Ustring;
   begin
      Append (Buffer, "<table>");
      for Month in Month_Number loop
         Append (Buffer, "<tr><td>");
         Append (Buffer, Filters.Month_Name_Da (Month));
         Append (Buffer, "<td>");
         for Day in Day_Number'First .. Last_Day_Of (Month) loop
            Append (Buffer, "<a href=");
            Append (Buffer, HREF_Image ((Generic_Year, Month, Day, 0.0)));
            Append (Buffer, ">");
            Append (Buffer, Day'Image);
            Append (Buffer, "</a>");
            Append (Buffer, New_Line);
         end loop;
         Append (Buffer, "</tr>");
         Append (Buffer, New_Line);
      end loop;
      Append (Buffer, "</table>");
      Append (Buffer, New_Line);
      return Buffer;
   end Build_TOC;

   ------------------
   -- Dispatch_TOC --
   ------------------

   function Dispatch_TOC (Request : Status_Data) return Response_Data
   is
      pragma Unreferenced (Request);
      use AWS, AWS.Templates, AWS.Response;

      File : constant String := WWW_Base & "html/toc.thtml";
      TOC  : constant Ustring := Build_TOC;
   begin
      Insert (Translations, Assoc ("WRAP", TOC));
      declare
         HTML : String        renames Parse (File, Translations);
         HTTP : Response_Data renames Build (AWS.MIME.Text_HTML,
                                             Message_Body => HTML);
      begin
         return HTTP;
      end;
   end Dispatch_TOC;

   -----------------
   -- Build_Daily --
   -----------------

   function Dispatch_Daily (Request : Status_Data) return Response_Data
   is
      use AWS, AWS.Templates, AWS.Response;

      subtype Param_List is Parameters.List;

      File : constant String := WWW_Base & "html/daily.thtml";

      List : Param_List    renames Status.Parameters (Request);
      Day  : String        renames Parameters.Get (List, "day");
      Tag  : String        renames Parameters.Get (List, "tag");
      Dag  : String        renames Parameters.Get (List, "dag");

      use Calendar;
      Concat : constant String := Day & Tag & Dag;  -- Expect only one is set
      Date   : constant String := (if
                                     Concat = "today" or
                                     Concat = "heute" or
                                     Concat = "idag"  or
                                     Concat = ""
                                   then Image (Clock)
                                   else Concat);
      Datum   : Time;
      Last    : Natural;
      Success : Boolean;
   begin
      Calendar.To_Date (Date, Last, Datum, Success);
      declare
         Number : constant Datum_Number := Number_Of (Datum);
         Point : Database.Data_Point renames Database.Base (Number);
      begin
         if Success then
            Insert (Translations, Assoc ("DATE", Date));
            Insert (Translations, Assoc ("COMMENT",      Point.Comment));
            Insert (Translations, Assoc ("VALUE",        Point.Value));
            Insert (Translations, Assoc ("URL_MONTH_EN",
                                         Filters.Month_Name_En (Datum)));
            Insert (Translations, Assoc ("URL_DAY",
                                         Filters.Day_Of (Datum)));
         else
            Insert (Translations,
                    Assoc ("DATE", "FEJL. Format: YYYY-MM-DD eller DD-MM-YYYY"));
            Insert (Translations, Assoc ("COMMENT", "FEJL"));
            Insert (Translations, Assoc ("VALUE",   "FEJL"));
         end if;

         declare
            HTML : String        renames Parse (File, Translations);
            HTTP : Response_Data renames Build (AWS.MIME.Text_HTML,
                                                Message_Body => HTML);
         begin
            return HTTP;
         end;
      end;
   end Dispatch_Daily;

   -------------------------
   -- Dispatch CSS_Action --
   -------------------------

   overriding function Dispatch
     (Handler : in CSS_Action;
      Request : in Status_Data) return Response_Data
   is
      use AWS;
      URI : constant String := Status.URI (Request);
   begin
      return Response.File (AWS.MIME.Text_CSS, WWW_Base & URI);
   end Dispatch;

end WWW_Manager;
