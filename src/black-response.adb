with
  Black.Text_IO;

package body Black.Response is
   function Not_Found (Resource : in String) return Class is
      use Text_IO;
      Message : constant String := "The requested resource, '" & Resource &
                                   "' was not found on the server.";
   begin
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 404 Not Found");
         Put_Line (R.Data, "Content-Length: 0");
         Put_Line (R.Data, "Content-Type: text/plain; charset=iso-8859-1");
         Put_Line (R.Data, "Content-Length:" & Natural'Image (Message'Length));
         New_Line (R.Data);
         Put      (R.Data, Message);

         R.Complete := True;
      end return;
   end Not_Found;

   function OK (Content_Type : in String;
                Data         : in Ada.Streams.Stream_Element_Array)
               return Class is
      use Text_IO;
   begin
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 200 OK");
         Put_Line (R.Data, "Content-Type: " & Content_Type);
         Put_Line (R.Data, "Content-Length:" & Natural'Image (Data'Length));
         New_Line (R.Data);
         R.Data.Append (Data);

         R.Complete := True;
      end return;
   end OK;

   function OK (Data : in String)
               return Class is
      use Text_IO;
   begin
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 200 OK");
         Put_Line (R.Data, "Content-Type: text/plain; charset=iso-8859-1");
         Put_Line (R.Data, "Content-Length:" & Natural'Image (Data'Length));
         New_Line (R.Data);
         Put      (R.Data, Data);

         R.Complete := True;
      end return;
   end OK;

   procedure Output_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Instance) is
      use Ada.Streams;
   begin
      if Item.Complete then
         for Index in Item.Data.First_Index .. Item.Data.Last_Index loop
            Stream.Write (Item.Data.Element (Index));
         end loop;
      else
         raise Constraint_Error
           with "Response object is not ready to be streamed.";
      end if;
   end Output_HTTP;

   function Redirect (Target    : in String;
                      Permanent : in Boolean)
                     return Class is
      use Text_IO;
   begin
      return R : Instance do
         if Permanent then
            Put_Line (R.Data, "HTTP/1.1 301 Moved Permanently");
         else
            Put_Line (R.Data, "HTTP/1.1 302 Moved Temporarily");
         end if;

         Put_Line (R.Data, "Location: " & Target);
         Put_Line (R.Data, "Content-Length: 0");
         New_Line (R.Data);

         R.Complete := True;
      end return;
   end Redirect;
end Black.Response;
