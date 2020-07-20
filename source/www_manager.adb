with AWS.MIME;
with AWS.Templates;

with Options;

package body WWW_Manager is

   WWW_Base : constant String := "www/";

   function Build_TOC   return Response_Data;
   function Build_Daily return Response_Data;

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

      if    URI = "/"       then  return Build_Daily;
      elsif URI = "/daglig" then  return Build_Daily;
      elsif URI = "/daily"  then  return Build_Daily;
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

   function Build_Daily return Response_Data
   is
      File_Name : constant String := WWW_Base & "html/daily.thtml";
      HTML      : String renames AWS.Templates.Parse (File_Name,
                                                      Translations);
      HTTP      : constant Response_Data := AWS.Response.Build (AWS.MIME.Text_HTML,
                                                                Message_Body => HTML);
   begin
      return HTTP;
   end Build_Daily;

end WWW_Manager;
