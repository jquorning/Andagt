--
--  GUD BEVARE DANMARK
--

with AWS.Dispatchers;
with AWS.Status;
with AWS.Response;

package WWW_Manager is

   subtype Status_Data   is AWS.Status.Data;
   subtype Response_Data is AWS.Response.Data;

   type WWW_Action is new AWS.Dispatchers.Handler with null record;

   overriding function Dispatch
     (Handler : in WWW_Action;
      Request : in Status_Data) return Response_Data;

   overriding function Clone (Element : in WWW_Action) return WWW_Action
   is (Element);


   type CSS_Action is new AWS.Dispatchers.Handler with null record;

   overriding function Dispatch
     (Handler : in CSS_Action;
      Request : in Status_Data) return Response_Data;

   overriding function Clone (Element : in CSS_Action) return CSS_Action
   is (Element);

end WWW_Manager;
