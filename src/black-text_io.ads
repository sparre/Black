with
  Ada.Streams,
  Ada.Strings.Unbounded;
with
  POSIX.IO;

private
package Black.Text_IO is
   procedure Put (File : in     POSIX.IO.File_Descriptor;
                  Item : in     String);
   procedure Put_Line (File : in     POSIX.IO.File_Descriptor;
                       Item : in     String);
   procedure New_Line (File : in     POSIX.IO.File_Descriptor);

   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return String;
   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Ada.Strings.Unbounded.Unbounded_String;
end Black.Text_IO;
