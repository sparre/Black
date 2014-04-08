with
  Ada.Streams,
  Ada.Strings.Unbounded;

private
package Black.Text_IO is
   procedure Put
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     String);
   procedure Put
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Ada.Strings.Unbounded.Unbounded_String);
   procedure Put
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Boolean);
   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     String);
   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Ada.Strings.Unbounded.Unbounded_String);
   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Boolean);
   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Integer);
   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Duration);
   procedure New_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class);

   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return String;
   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Ada.Strings.Unbounded.Unbounded_String;
end Black.Text_IO;
