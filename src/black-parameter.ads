with
  Ada.Strings.Unbounded;

package Black.Parameter is

   No_Such_Parameter_Key   : exception;
   No_Such_Parameter_Value : exception;

   type Instance (With_Value : Boolean := True) is
      record
         Key : Ada.Strings.Unbounded.Unbounded_String;
         case With_Value is
            when True =>
               Value : Ada.Strings.Unbounded.Unbounded_String;
            when False =>
               null;
         end case;
      end record;

end Black.Parameter;
