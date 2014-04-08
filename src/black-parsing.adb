with
  Ada.IO_Exceptions,
  Ada.Strings,
  Ada.Strings.Fixed;
with
  Black.Text_IO;

package body Black.Parsing is
   function End_Of_Header (Item : in Header) return Boolean is
   begin
      return Ada.Strings.Unbounded.Length (Item.Current_Line) = 0;
   end End_Of_Header;

   function Get (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                return Header is
   begin
      return (Previous_Line => <>,
              Current_Line  => Text_IO.Get_Line (Stream));
   end Get;

   function Key (Item : in Header_Line) return HTTP.Header_Key is
      use Ada.Strings.Unbounded;
   begin
      return HTTP.Header_Key (Slice (Item.Line, 1, Item.Split_Position - 1));
   end Key;

   function Parse (Line : in Ada.Strings.Unbounded.Unbounded_String)
                  return Header_Line is
      use Ada.Strings.Unbounded;
      Split_Position : constant Natural := Index (Line, ": ");
   begin
      if Split_Position = 0 then
         raise Protocol_Error
           with "Can not split """ & To_String (Line) & """ in key and value.";
      else
         return (Line           => Line,
                 Split_Position => Split_Position);
      end if;
   end Parse;

   function Parse (Line : Ada.Strings.Unbounded.Unbounded_String)
                  return HTTP.Statuses is
      use Ada.Strings.Unbounded;
      Head    : constant String := HTTP.Version & " ### ";
      Compare : constant String := Slice (Line, 1, Head'Length);
   begin
      for Status in HTTP.Statuses loop
         if Compare = HTTP.Status_Line (Status) (1 .. Compare'Length) then
            return Status;
         end if;
      end loop;

      raise Protocol_Error
        with "Unexpected " & HTTP.Version & " response: " & To_String (Line);
   end Parse;

   procedure Read (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   From   : in out Header;
                   Item   :    out Header_Line) is
      use Ada.Strings.Unbounded;
   begin
      if End_Of_Header (From) then
         raise Ada.IO_Exceptions.End_Error;
      else
         From.Previous_Line := From.Current_Line;
         loop
            From.Current_Line  := Text_IO.Get_Line (Stream);
            exit when End_Of_Header (From);

            if Element (From.Current_Line, 1) = ' ' then
               From.Previous_Line := From.Previous_Line & From.Current_Line;
            else
               exit;
            end if;
         end loop;
         Item := Parse (From.Previous_Line);
      end if;
   end Read;

   function Value (Item : in Header_Line) return String is
      use Ada.Strings, Ada.Strings.Fixed, Ada.Strings.Unbounded;
   begin
      return
        Trim (Slice (Item.Line, Item.Split_Position + 2, Length (Item.Line)),
              Both);
   end Value;

   function Value (Item : in Header_Line)
                  return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Strings, Ada.Strings.Unbounded;
   begin
      return Trim (Unbounded_Slice (Item.Line,
                                    Item.Split_Position + 2,
                                    Length (Item.Line)),
                   Both);
   end Value;

   function Value (Item : in Header_Line) return Integer is
   begin
      return Integer'Value (Item.Value);
   end Value;

   function Value (Item : in Header_Line) return Boolean is
   begin
      return Boolean'Value (Item.Value);
   end Value;

   function Value (Item : in Header_Line) return Duration is
   begin
      return Duration'Value (Item.Value);
   end Value;
end Black.Parsing;
