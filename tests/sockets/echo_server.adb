with
  Ada.Command_Line,
  Ada.Exceptions,
  Ada.Text_IO;
with
  POSIX.IO;
with
  GNAT.Sockets;
with
  GNAT.Sockets.Compatibility,
  GNAT.Sockets.Convenience;

procedure Echo_Server is
   Stop : exception;

   procedure Handle (Connection : in     GNAT.Sockets.Socket_Type);
   procedure Handle (Connection : in     GNAT.Sockets.Socket_Type) is
      use Ada.Text_IO;
      Socket : constant POSIX.IO.File_Descriptor :=
        GNAT.Sockets.Compatibility.POSIX_File_Descriptor (Connection);

      Buffer : POSIX.IO.File_Descriptor;
      pragma Unreferenced (Buffer);
      --  We don't check if the duplication works.

   begin
      Buffer := POSIX.IO.Duplicate_and_Close
                  (File   => Socket,
                   Target => POSIX.IO.Standard_Input);
      Buffer := POSIX.IO.Duplicate_and_Close
                  (File   => Socket,
                   Target => POSIX.IO.Standard_Output);

      loop
         declare
            Line : constant String := Get_Line;
         begin
            exit when Line = "EOF";
            if Line = "STOP" then raise Stop; end if;
            Put_Line (Line);
         end;
      end loop;

      POSIX.IO.Close (POSIX.IO.Standard_Input);
      POSIX.IO.Close (POSIX.IO.Standard_Output);
      POSIX.IO.Close (Socket);
   exception
      when End_Error =>
         POSIX.IO.Close (POSIX.IO.Standard_Input);
         POSIX.IO.Close (POSIX.IO.Standard_Output);
         POSIX.IO.Close (Socket);
      when Stop =>
         POSIX.IO.Close (POSIX.IO.Standard_Input);
         POSIX.IO.Close (POSIX.IO.Standard_Output);
         POSIX.IO.Close (Socket);
         raise;
   end Handle;
begin
   Disabled :
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count = 1 and then Argument (1) = "--no-run" then
         Ada.Text_IO.Put_Line ("Not running this time.");
         Set_Exit_Status (Success);
         return;
      end if;
   end Disabled;

   Echo :
   declare
      use GNAT.Sockets;
      Server : constant Socket_Type := Convenience.Make_Server (Port => 8042);
   begin
      loop
         declare
            Connection : GNAT.Sockets.Socket_Type;
            Address    : GNAT.Sockets.Sock_Addr_Type;
         begin
            GNAT.Sockets.Accept_Socket (Server  => Server,
                                        Socket  => Connection,
                                        Address => Address);
            Handle (Connection);
         end;
      end loop;
   end Echo;
exception
   when Stop =>
      Ada.Text_IO.Put_Line ("Shutting down on request from client.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   when E : others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => "Exception raised in echo server: " &
                 Ada.Exceptions.Exception_Message (E));
      raise;
end Echo_Server;
