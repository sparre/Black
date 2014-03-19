with
  Black.Tests.Text_IO;

package body Black.Tests is
   function Suite return Ahven.Framework.Test_Suite is
      use Ahven.Framework;
      Text_IO_Test : Black.Tests.Text_IO.Test;
   begin
      return Suite : Test_Suite := Create_Suite ("Black") do
         Add_Static_Test (Suite, Text_IO_Test);
      end return;
   end Suite;
end Black.Tests;
