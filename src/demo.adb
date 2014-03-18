with
  Ada.Text_IO;
with
  Black.HTTP.Request,
  Black.HTTP.Server;
procedure Demo is
   use Black;
begin
   Ada.Text_IO.Put_Line ("Demo started");

   declare
      procedure Redirect_To_Jacob (Request : in out HTTP.Request.Instance);
      procedure Redirect_To_Jacob (Request : in out HTTP.Request.Instance) is
         use HTTP.Request;
      begin
         Redirect (Request   => Request,
                   Target    => "http://www.jacob-sparre.dk/",
                   Permanent => False);
      end Redirect_To_Jacob;

      package Server is
         new Black.HTTP.Server (Handler => Redirect_To_Jacob);
   begin
      Server.Start (Listen_To => 8042);
      Ada.Text_IO.Put_Line ("HTTP server created");
      delay 86_000.0;
      Server.Stop;
   end;
end Demo;
