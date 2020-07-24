--
--  GUD BEVARE DANMARK
--

with AWS.Config;
with AWS.Server;
with AWS.Services.Dispatchers.URI;

with WWW_Manager;
with Filters;

package body HTTP_Server is

   use AWS;

   HTTP : Server.HTTP;
   Conf : Config.Object;
   Root : Services.Dispatchers.URI.Handler;

   WWW  : WWW_Manager.WWW_Action;
   CSS  : WWW_Manager.CSS_Action;

   -------------
   -- Startup --
   -------------

   procedure Startup is
      use AWS.Services.Dispatchers.URI;
   begin
      Register (Root, "/",       WWW);
      Register (Root, "/toc",    WWW);
      Register (Root, "/daglig", WWW);
      Register (Root, "/daily",  WWW);

      Register_Regexp (Root, ".*\.css",  CSS);

      Filters.Register;

      AWS.Server.Start (HTTP, Root, Conf);
   end Startup;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      AWS.Server.Shutdown (HTTP);
   end Shutdown;

   ----------
   -- Wait --
   ----------

   procedure Wait is
      use AWS.Server;
   begin
      Wait (Q_Key_Pressed);
   end Wait;


end HTTP_Server;
