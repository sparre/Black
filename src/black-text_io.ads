with
  POSIX.IO;
private
package Black.Text_IO is
   procedure Put (File : in     POSIX.IO.File_Descriptor;
                  Item : in     String);
   procedure Put_Line (File : in     POSIX.IO.File_Descriptor;
                       Item : in     String);
   procedure New_Line (File : in     POSIX.IO.File_Descriptor);
end Black.Text_IO;
