with
  Ada.Characters.Latin_1,
  Ada.Streams;
package body Black.Text_IO is
   procedure New_Line (File : in     POSIX.IO.File_Descriptor) is
   begin
      Put (File => File,
           Item => Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF);
   end New_Line;

   procedure Put (File : in     POSIX.IO.File_Descriptor;
                  Item : in     String) is
      use type Ada.Streams.Stream_Element_Offset;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Item'Length);
      for Buffer'Address use Item'Address;
      pragma Assert (Buffer'Size = Item'Size);
      Last : Ada.Streams.Stream_Element_Offset := Buffer'First;
   begin
      while Last in Buffer'Range loop
         POSIX.IO.Write (File   => File,
                         Buffer => Buffer (Last .. Buffer'Last),
                         Last   => Last);
         Last := Last + 1;
      end loop;
   end Put;

   procedure Put_Line (File : in     POSIX.IO.File_Descriptor;
                       Item : in     String) is
   begin
      Put (File => File,
           Item => Item &
                   Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF);
   end Put_Line;
end Black.Text_IO;
