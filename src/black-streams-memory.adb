--  Black HTTP and Websocket library - License
--
--  Copyright (c) 2014, AdaHeads K/S
--  Copyright (c) 2016, JSA Research & Innovation
--  Copyright (c) 2017, Consafe Logistics A/S
--
--  Permission to use, copy, modify, and distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

with Ada.Unchecked_Conversion;

package body Black.Streams.Memory is

   not overriding
   function Length (Stream : in Instance)
                   return Ada.Streams.Stream_Element_Offset is
      use Ada.Streams;
   begin
      return Stream_Element_Offset (Stream.Data.Length) * Buffer_Type'Length
        - (Stream.Head - Buffer_Type'First)
        - (Buffer_Type'Last - Stream.Tail);
   end Length;

   not overriding
   function Length (Stream : in Instance) return Natural is
     (Natural (Ada.Streams.Stream_Element_Offset'(Stream.Length)));

   overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out Ada.Streams.Stream_Element_Array;
                   Last   :    out Ada.Streams.Stream_Element_Offset) is
      use Ada.Streams;
      use type Ada.Containers.Count_Type;
   begin
      Last := Item'First - 1;

      loop
         exit when Stream.Data.Is_Empty;
         exit when Stream.Data.Length = 1 and then Stream.Head > Stream.Tail;
         exit when Last >= Item'Last;

         declare
            First_Element : Stream_Element_Array renames Stream.Data.First_Element;
            Remaining     : Stream_Element_Array renames First_Element (Stream.Head .. (if Stream.Data.Length > 1 then
                                                                                           First_Element'Last
                                                                                        else
                                                                                           Stream.Tail));
            Target        : Stream_Element_Array renames Item (Last + 1 .. Item'Last);
            Copy          : constant Stream_Element_Offset := Stream_Element_Offset'Min (Remaining'Length, Target'Length);
         begin
            Target (Target'First .. Target'First + Copy - 1) :=
              Remaining (Remaining'First .. Remaining'First + Copy - 1);

            Last        := Last        + Copy;
            Stream.Head := Stream.Head + Copy;

            if Stream.Head > Buffer_Type'Last then
               Stream.Data.Delete_First;
               Stream.Head := Buffer_Type'First;
            end if;
         end;
      end loop;

      if Stream.Data.Is_Empty then
         Stream.Head := Buffer_Type'First;
         Stream.Tail := Buffer_Type'Last;
      end if;
   end Read;

   not overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out String;
                   Last   :    out Natural) is
      use Ada.Streams;

      Buffer        : Stream_Element_Array (1 .. Item'Length);
      Read_Elements : Stream_Element_Offset;

      pragma Assert (Buffer'Size = Item'Size);
      pragma Assert (Buffer'Component_Size = Item'Component_Size);
   begin
      Stream.Read (Item => Buffer,
                   Last => Read_Elements);

      declare
         subtype Stream_Index_Range is Stream_Element_Count range 1 .. Read_Elements;
         subtype String_Index_Range is Positive             range 1 .. Natural (Read_Elements);

         subtype Fixed_Stream_Element_Buffer is Stream_Element_Array (Stream_Index_Range);
         subtype Fixed_Character_Buffer      is String               (String_Index_Range);

         function "+" is new Ada.Unchecked_Conversion (Source => Fixed_Stream_Element_Buffer,
                                                       Target => Fixed_Character_Buffer);

         Stream_View : Fixed_Stream_Element_Buffer renames Buffer (Stream_Index_Range);
         String_View : Fixed_Character_Buffer      renames Item   (String_Index_Range);
      begin
         String_View := +Stream_View;
         Last := Item'First + String_View'Length - 1;
      end;
   end Read;

   not overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out Ada.Strings.Unbounded.Unbounded_String) is
      Buffer : String (1 .. Stream.Length);
      Last   : Natural;
   begin
      Stream.Read (Item => Buffer,
                   Last => Last);

      pragma Assert (Stream.Length = 0);
      pragma Assert (Last = Buffer'Last);

      Item := Ada.Strings.Unbounded.To_Unbounded_String (Buffer);
   end Read;

   overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     Ada.Streams.Stream_Element_Array) is
      use Ada.Streams;

      Next : Stream_Element_Offset := Item'First;
   begin
      loop
         exit when Next > Item'Last;

         if Stream.Tail = Buffer_Type'Last then
            declare
               Buffer           : Buffer_Type;
               Elements_To_Copy : constant Stream_Element_Count := Stream_Element_Offset'Min (Buffer_Type'Length,
                                                                                              Item'Last - Next + 1);
               Source           : Stream_Element_Array renames Item   (Next         .. Next         + Elements_To_Copy - 1);
               Target           : Stream_Element_Array renames Buffer (Buffer'First .. Buffer'First + Elements_To_Copy - 1);
            begin
               Target := Source;
               Stream.Data.Append (Buffer);
               Stream.Tail := Target'Last;
               Next := Next + Elements_To_Copy;
            end;
         else
            declare
               Buffer           : Buffer_Type := Stream.Data.Last_Element;
               Elements_To_Copy : constant Stream_Element_Count := Stream_Element_Offset'Min (Buffer_Type'Length - Stream.Tail,
                                                                                              Item'Last - Next + 1);
               Source           : Stream_Element_Array renames Item   (Next            .. Next            + Elements_To_Copy - 1);
               Target           : Stream_Element_Array renames Buffer (Stream.Tail + 1 .. Stream.Tail + 1 + Elements_To_Copy - 1);
            begin
               Target := Source;
               Stream.Data.Replace_Element (Position => Stream.Data.Last,
                                            New_Item => Buffer);
               Stream.Tail := Target'Last;
               Next := Next + Elements_To_Copy;
            end;
         end if;
      end loop;
   end Write;

   not overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     String) is
      use Ada.Strings.Unbounded;
      use Ada.Streams;

      pragma Assert (Stream_Element_Array'Component_Size = String'Component_Size, "Character size /= stream element size.");

      subtype String_Index_Range is Positive             range 1 .. Item'Length;
      subtype Stream_Index_Range is Stream_Element_Count range 1 .. Item'Length;

      subtype Fixed_Character_Buffer      is String               (String_Index_Range);
      subtype Fixed_Stream_Element_Buffer is Stream_Element_Array (Stream_Index_Range);

      function "+" is new Ada.Unchecked_Conversion (Source => Fixed_Character_Buffer,
                                                    Target => Fixed_Stream_Element_Buffer);

      As_Characters : Fixed_Character_Buffer renames Item;
   begin
      Stream.Write (Item => +As_Characters);
   end Write;

   not overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     Ada.Strings.Unbounded.Unbounded_String) is
      use Ada.Strings.Unbounded;
      use Ada.Streams;

      pragma Assert (Stream_Element_Array'Component_Size = String'Component_Size, "Character size /= stream element size.");

      subtype String_Index_Range is Positive             range 1 .. Length (Item);
      subtype Stream_Index_Range is Stream_Element_Count range 1 .. Stream_Element_Offset (Length (Item));

      subtype Fixed_Character_Buffer      is String               (String_Index_Range);
      subtype Fixed_Stream_Element_Buffer is Stream_Element_Array (Stream_Index_Range);

      function "+" is new Ada.Unchecked_Conversion (Source => Fixed_Character_Buffer,
                                                    Target => Fixed_Stream_Element_Buffer);

      As_Characters : constant Fixed_Character_Buffer := To_String (Item);
   begin
      Stream.Write (Item => +As_Characters);
   end Write;

end Black.Streams.Memory;
