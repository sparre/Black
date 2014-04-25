private
generic
   type Content_Type is private;
package Black.Generic_Optional is
   type Instance (Set : Boolean := False) is
      record
         case Set is
            when True =>
               Value : Content_Type;
            when False =>
               null;
         end case;
      end record;

   function "+" (Item : in Content_Type) return Instance;

   function "=" (Left  : in Content_Type;
                 Right : in Instance) return Boolean;
   function "=" (Left  : in Instance;
                 Right : in Content_Type) return Boolean;
end Black.Generic_Optional;
