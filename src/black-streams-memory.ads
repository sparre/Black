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

with Ada.Streams;
with Ada.Strings.Unbounded;

private with Ada.Containers.Doubly_Linked_Lists;

package Black.Streams.Memory is

   type Instance is new Ada.Streams.Root_Stream_Type with private;

   not overriding
   function Length (Stream : in Instance)
                   return Ada.Streams.Stream_Element_Offset;

   not overriding
   function Length (Stream : in Instance) return Natural;

   overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out Ada.Streams.Stream_Element_Array;
                   Last   :    out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     Ada.Streams.Stream_Element_Array);

   not overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out String;
                   Last   :    out Natural);

   not overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out Ada.Strings.Unbounded.Unbounded_String);

   not overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     String);

   not overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     Ada.Strings.Unbounded.Unbounded_String);

private

   Buffer_Size : constant := 16_000;

   subtype Buffer_Type is Ada.Streams.Stream_Element_Array (1 .. Buffer_Size);

   package Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Buffer_Type,
                                              "="          => Ada.Streams."=");

   type Instance is new Ada.Streams.Root_Stream_Type with
      record
         Data : Lists.List;
         Head : Ada.Streams.Stream_Element_Offset := Buffer_Type'First;
         Tail : Ada.Streams.Stream_Element_Offset := Buffer_Type'Last;
      end record;

end Black.Streams.Memory;
