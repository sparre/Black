with
  Black.Tests.Request,
  Black.Tests.Response,
  Black.Tests.Text_IO;

package body Black.Tests is
   function Suite return Ahven.Framework.Test_Suite is
      use Ahven.Framework;
      Request_Test  : Black.Tests.Request.Test;
      Response_Test : Black.Tests.Response.Test;
      Text_IO_Test  : Black.Tests.Text_IO.Test;
   begin
      return Suite : Test_Suite := Create_Suite ("Black") do
         Add_Static_Test (Suite, Request_Test);
         Add_Static_Test (Suite, Response_Test);
         Add_Static_Test (Suite, Text_IO_Test);
      end return;
   end Suite;
end Black.Tests;
