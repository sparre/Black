with
  Ada.Exceptions,
  Ada.Text_IO;
with
  GNAT.Sockets;
with
  GNAT.Sockets.Convenience;
with
  Black.HTTP.Request.Constructors;
package body Black.HTTP.Server is
   task Listener is
      entry Start (Listen_To : in TCP_Ports);
      entry Stop;
   end Listener;

   procedure Handle (Connection : in     GNAT.Sockets.Socket_Type);

   procedure Handle (Connection : in     GNAT.Sockets.Socket_Type) is
      Request : HTTP.Request.Instance :=
                  HTTP.Request.Constructors.From (Connection);
   begin
      Handler (Request);

      if not HTTP.Request.Committed (Request) then
         HTTP.Request.Commit (Request);
      end if;
   exception
      when Occurred_Error : others =>
         if not HTTP.Request.Committed (Request) then
            HTTP.Request.Server_Error (Request   => Request,
                                       Error     => Occurred_Error);
         end if;
   end Handle;

   procedure Start (Listen_To : in TCP_Ports := 80) is
   begin
      Listener.Start (Listen_To => Listen_To);
   end Start;

   procedure Stop is
   begin
      Listener.Stop;
   end Stop;

   task body Listener is
      use Ada.Text_IO;
      use GNAT;
      Server : GNAT.Sockets.Socket_Type;
   begin
      Put_Line ("HTTP server task started.");

      declare
         Port : TCP_Ports;
      begin
         accept Start (Listen_To : in TCP_Ports) do
            Port := Listen_To;
         end Start;

         Put_Line (Item => "HTTP server task will listen on port" &
                           TCP_Ports'Image (Port) & ".");

         Server := GNAT.Sockets.Convenience.Make_Server
                     (Port => GNAT.Sockets.Port_Type (Port));

         Put_Line (Item => "HTTP server task ready to answer requests.");
      end;

      loop
         declare
            Connection : GNAT.Sockets.Socket_Type;
            Address    : GNAT.Sockets.Sock_Addr_Type;
         begin
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item => "Waiting for connection...");

            select
               accept Stop;
            else
               GNAT.Sockets.Accept_Socket (Server  => Server,
                                           Socket  => Connection,
                                           Address => Address);
               Handle (Connection);
            end select;
         end;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Exception raised in a Black HTTP server task: " &
                    Ada.Exceptions.Exception_Message (E));
   end Listener;
end Black.HTTP.Server;
