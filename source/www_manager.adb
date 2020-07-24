--
--  GUD BEVARE DANMARK
--

with AWS.MIME;
with AWS.Templates;
with AWS.Parameters;

with Options;
with Calendar;
with Database;
with Filters;

package body WWW_Manager is

   WWW_Base : constant String := "www/";

   subtype Time is Calendar.Time;

   function Build_TOC   return Response_Data;
   function Build_Daily (Request : Status_Data) return Response_Data;

   Translations : AWS.Templates.Translate_Set;

   --------------
   -- Diapatch --
   --------------

   overriding function Dispatch
     (Handler : in WWW_Action;
      Request : in Status_Data) return Response_Data
   is
      use AWS;
      URI : constant String := Status.URI (Request);
   begin
      AWS.Templates.Insert (Translations, AWS.Templates.Assoc ("ANDAGTSBOG", Options.File_Name));

      if    URI = "/"       then  return Build_Daily (Request);
      elsif URI = "/daglig" then  return Build_Daily (Request);
      elsif URI = "/daily"  then  return Build_Daily (Request);
      else                        return Build_TOC;
      end if;
   end Dispatch;

   ---------------
   -- Build_TOC --
   ---------------

   function Build_TOC return Response_Data
   is
      File_Name : constant String := WWW_Base & "html/toc.thtml";
      HTML      : String renames AWS.Templates.Parse (File_Name,
                                                      Translations);
      HTTP      : constant Response_Data := AWS.Response.Build (AWS.MIME.Text_HTML,
                                                                Message_Body => HTML);
   begin
      return HTTP;
   end Build_TOC;

   -----------------
   -- Build_Daily --
   -----------------

   function Build_Daily (Request : Status_Data) return Response_Data
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
   end Build_Daily;

end WWW_Manager;
