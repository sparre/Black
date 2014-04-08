with
  Ada.Characters.Handling,
  Ada.Characters.Latin_1,
  Ada.IO_Exceptions,
  Ada.Strings,
  Ada.Strings.Fixed;

package body Black.Text_IO is
   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return String is
   begin
      return Ada.Strings.Unbounded.To_String (Get_Line (Stream));
   end Get_Line;

   function Get_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Ada.Strings.Unbounded.Unbounded_String is
      use Ada.Characters.Latin_1, Ada.Strings.Unbounded;
      Buffer : Unbounded_String := Null_Unbounded_String;
      Next   : Character;
   begin
      loop
         Character'Read (Stream, Next);

         if Next = CR then
            Character'Read (Stream, Next);
            if Next = LF then
               return Buffer;
            else
               Append (Buffer, CR & Next);
            end if;
         else
            Append (Buffer, Next);
         end if;
      end loop;
   exception
      when Ada.IO_Exceptions.End_Error =>
         if Buffer = Null_Unbounded_String then
            raise;
         else
            return Buffer;
         end if;
   end Get_Line;

   procedure New_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class) is
   begin
      Put (Target => Target,
           Item   => Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF);
   end New_Line;

   procedure Put
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     String) is
      pragma Assert (Ada.Streams.Stream_Element'Size = Character'Size);
   begin
      String'Write (Target, Item);
   end Put;

   procedure Put
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Put (Target => Target,
           Item   => Ada.Strings.Unbounded.To_String (Item));
   end Put;

   procedure Put
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Boolean) is
      use Ada.Characters.Handling;
   begin
      Put (Target => Target,
           Item   => To_Lower (Boolean'Image (Item)));
   end Put;

   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     String) is
   begin
      Put (Target => Target,
           Item   => Item &
                     Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF);
   end Put_Line;

   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Put_Line (Target => Target,
                Item   => Ada.Strings.Unbounded.To_String (Item));
   end Put_Line;

   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Boolean) is
      use Ada.Characters.Handling;
   begin
      Put_Line (Target => Target,
                Item   => To_Lower (Boolean'Image (Item)));
   end Put_Line;

   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Integer) is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      Put_Line (Target => Target,
                Item   => Trim (Integer'Image (Item), Both));
   end Put_Line;

   procedure Put_Line
     (Target : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Duration) is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      Put_Line (Target => Target,
                Item   => Trim (Duration'Image (Item), Both));
   end Put_Line;
end Black.Text_IO;
