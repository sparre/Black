with
  GNAT.Sockets.Compatibility;
package body Black.HTTP.Request.Constructors is
   function From (Connection : in GNAT.Sockets.Socket_Type)
                 return Black.HTTP.Request.Instance is
   begin
      return Result : Request.Instance do
         Result.Connection :=
           GNAT.Sockets.Compatibility.POSIX_File_Descriptor (Connection);
         Result.Committed  := False;
      end return;
   end From;
end Black.HTTP.Request.Constructors;
