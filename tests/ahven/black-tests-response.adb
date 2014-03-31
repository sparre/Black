with
  Black.Response;

package body Black.Tests.Response is
   procedure HTML_Document is
   begin
      Ahven.Skip (Message => "Not implemented yet.");
   end HTML_Document;

   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("HTTP responses");

      pragma Style_Checks ("M100"); --  Readable table

      Add_Test_Routine (T,  Redirection'Access,          "Redirection");
      Add_Test_Routine (T,  Plain_Text_Document'Access,  "Plain text document");
      Add_Test_Routine (T,  HTML_Document'Access,        "HTML document");
      Add_Test_Routine (T,  JSON_Document'Access,        "JSON document");
      Add_Test_Routine (T,  Nonexistent_Resource'Access, "Non-existent resource");
      Add_Test_Routine (T,  Websocket'Access,            "Websocket");

      pragma Style_Checks ("M79"); --  Standard line length
   end Initialize;

   procedure JSON_Document is
   begin
      Ahven.Skip (Message => "Not implemented yet.");
   end JSON_Document;

   procedure Nonexistent_Resource is
   begin
      declare
         O : Black.Response.Class :=
               Black.Response.Not_Found (Resource => "/not-here/");
         pragma Unreferenced (O);
      begin
         null;
      end;
   end Nonexistent_Resource;

   procedure Plain_Text_Document is
   begin
      declare
         O : Black.Response.Class := Black.Response.OK (Data => "Hello!");
         pragma Unreferenced (O);
      begin
         null;
      end;
   end Plain_Text_Document;

   procedure Redirection is
   begin
      declare
         P : Black.Response.Class :=
               Black.Response.Redirect (Target    => "http://sj.se/",
                                        Permanent => True);
         T : Black.Response.Class :=
               Black.Response.Redirect (Target    => "http://sj.se/",
                                        Permanent => False);
         pragma Unreferenced (P, T);
      begin
         null;
      end;
   end Redirection;

   procedure Websocket is
   begin
      declare
         O : Black.Response.Class :=
               Black.Response.Switch_To_Websocket (Key => "AAA");
         pragma Unreferenced (O);
      begin
         null;
      end;
   end Websocket;
end Black.Tests.Response;
