with
  Ahven.Framework;

package Black.Tests.Text_IO is
   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);
private
   procedure Put_Test;
   procedure Put_Line_Test;
   procedure New_Line_Test;
   procedure Get_Line_Test;
end Black.Tests.Text_IO;
