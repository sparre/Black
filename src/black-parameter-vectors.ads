with
  Ada.Containers.Indefinite_Vectors;

package Black.Parameter.Vectors is
   new Ada.Containers.Indefinite_Vectors
         (Index_Type   => Positive,
          Element_Type => Instance);
