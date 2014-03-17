--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

package body GNAT.Sockets.Compatibility is
   function POSIX_File_Descriptor
     (Socket : in Socket_Type) return POSIX.IO.File_Descriptor is
   begin
      return POSIX.IO.File_Descriptor (Socket);
   end POSIX_File_Descriptor;
end GNAT.Sockets.Compatibility;
