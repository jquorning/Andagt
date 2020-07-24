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


end HTTP_Server;
