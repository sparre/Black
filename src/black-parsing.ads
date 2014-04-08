with
  Ada.Streams,
  Ada.Strings.Unbounded;
with
  Black.HTTP;

private
package Black.Parsing is
   function Parse (Line : Ada.Strings.Unbounded.Unbounded_String)
                  return HTTP.Statuses;

   type Header is private;
   function Get (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                return Header;
   function End_Of_Header (Item : in Header) return Boolean;

   type Header_Line is tagged private;
   procedure Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   From   : in out Header;
                   Item   :    out Header_Line);
   function Parse (Line : in Ada.Strings.Unbounded.Unbounded_String)
                  return Header_Line;
   function Key   (Item : in Header_Line) return HTTP.Header_Key;
   function Value (Item : in Header_Line) return String;
   function Value (Item : in Header_Line)
                  return Ada.Strings.Unbounded.Unbounded_String;
   function Value (Item : in Header_Line) return Integer;
   function Value (Item : in Header_Line) return Boolean;
   function Value (Item : in Header_Line) return Duration;
private
   type Header is
      record
         Previous_Line : Ada.Strings.Unbounded.Unbounded_String;
         Current_Line  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Header_Line is tagged
      record
         Line           : Ada.Strings.Unbounded.Unbounded_String;
         Split_Position : Positive;
      end record;
end Black.Parsing;
