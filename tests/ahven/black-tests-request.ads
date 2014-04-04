with
  Ahven.Framework;

package Black.Tests.Request is
   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
private
   procedure Output_Input_Test;
end Black.Tests.Request;
