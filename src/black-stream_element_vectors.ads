with
  Ada.Containers.Indefinite_Vectors,
  Ada.Streams;

private
package Black.Stream_Element_Vectors is
  new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Positive,
         Element_Type => Ada.Streams.Stream_Element_Array,
         "="          => Ada.Streams."=");
