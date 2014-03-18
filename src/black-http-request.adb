with
  Ada.IO_Exceptions;
with
  Black.Text_IO;
package body Black.HTTP.Request is
   procedure Commit (Request : in out Instance) is
   begin
      raise Program_Error
        with "Requests contain no internal, committable state.";
   end Commit;

   function Committed (Request : in Instance) return Boolean is
   begin
      return Request.Committed;
   end Committed;

   function Method (Request : in Instance) return Methods is
   begin
      raise Program_Error
        with "We have to read the actual request first.";
      return Get;
   end Method;

   procedure Redirect (Request   : in out Instance;
                       Target    : in     String;
                       Permanent : in     Boolean) is
      use Black.Text_IO;
   begin
      if Request.Committed then
         raise Ada.IO_Exceptions.Use_Error
           with "The response to the request has already been committed.";
      else
         if Permanent then
            Put_Line (Request.Connection, "HTTP/1.1 301 Moved Permanently");
         else
            Put_Line (Request.Connection, "HTTP/1.1 302 Moved Temporarily");
         end if;
         Put_Line (Request.Connection, "Server: Black");
         Put_Line (Request.Connection, "Location: " & Target);
         New_Line (Request.Connection);
         Request.Committed := True;
      end if;
   end Redirect;

   function Resource (Request : in Instance) return String is
   begin
      raise Program_Error
        with "We have to read the actual request first.";
      return "/";
   end Resource;

   procedure Server_Error
     (Request : in out Instance;
      Error   : in     Ada.Exceptions.Exception_Occurrence) is
      use Ada.Exceptions;
      use Black.Text_IO;
   begin
      if Request.Committed then
         null; --  Too late to report a server error.
      else
         Put_Line (File => Request.Connection,
                   Item => "HTTP/1.1 500 Server Error");
         Put_Line (File => Request.Connection,
                   Item => "Server: Black");
         Put_Line (File => Request.Connection,
                   Item => "Content-Type: text/plain; charset=iso-8859-1");
         New_Line (File => Request.Connection);
         Put_Line (File => Request.Connection,
                   Item => "Internal server error (exception):" &
                           Exception_Name (Error) & " (" &
                           Exception_Message (Error) & ")");
         New_Line (File => Request.Connection);

         Request.Committed := True;
      end if;
   end Server_Error;

   function URL (Request : in Instance) return String is
   begin
      raise Program_Error
        with "We have to read the actual request first.";
      return "http://localhost/";
   end URL;
end Black.HTTP.Request;
