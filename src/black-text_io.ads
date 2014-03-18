with
  Ada.Streams,
  Ada.Strings.Unbounded;
with
  Black.Stream_Element_Vectors;

private
package Black.Text_IO is
   procedure Put (Target : in out Stream_Element_Vectors.Vector;
                  Item   : in     String);
   procedure Put_Line (Target : in out Stream_Element_Vectors.Vector;
                       Item   : in     String);
   procedure New_Line (Target : in out Stream_Element_Vectors.Vector);

   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return String;
   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Ada.Strings.Unbounded.Unbounded_String;
end Black.Text_IO;
