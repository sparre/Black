with
  Ahven.Framework;

package Black.Tests.Response is
   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
private
   procedure Redirection;
   procedure Plain_Text_Document;
   procedure HTML_Document;
   procedure JSON_Document;
   procedure Nonexistent_Resource;
   procedure Websocket;
end Black.Tests.Response;
