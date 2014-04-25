package body Black.Generic_Optional is

   function "+" (Item : in Content_Type) return Instance is
   begin
      return (Set   => True,
              Value => Item);
   end "+";

   function "=" (Left  : in Content_Type;
                 Right : in Instance) return Boolean is
   begin
      return Right.Set and then Left = Right.Value;
   end "=";

   function "=" (Left  : in Instance;
                 Right : in Content_Type) return Boolean is
   begin
      return Left.Set and then Left.Value = Right;
   end "=";

end Black.Generic_Optional;
