with
  Ada.Streams;
private
with
  Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Black.Streams.Memory is
   type Instance is new Ada.Streams.Root_Stream_Type with private;

   overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out Ada.Streams.Stream_Element_Array;
                   Last   :    out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     Ada.Streams.Stream_Element_Array);

   not overriding
   function Copy (Stream : in Instance) return Instance;
private
   package Lists is
      new Ada.Containers.Indefinite_Doubly_Linked_Lists
            (Element_Type => Ada.Streams.Stream_Element_Array,
             "="          => Ada.Streams."=");

   type Instance is new Ada.Streams.Root_Stream_Type with
      record
         Data : Lists.List;
         Next : Ada.Streams.Stream_Element_Offset;
      end record;
end Black.Streams.Memory;
