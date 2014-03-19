with
  Ahven.Framework;

package Black.Text_IO_Tests is
   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
private
   procedure Put_Test;
   procedure Put_Line_Test;
   procedure New_Line_Test;
end Black.Text_IO_Tests;
