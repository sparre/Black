with
  Ahven.Framework,
  Ahven.Text_Runner;
with
  Black.Text_IO_Tests;

procedure Runner is
   Suite : Ahven.Framework.Test_Suite :=
     Ahven.Framework.Create_Suite ("All my tests");
begin
   Ahven.Framework.Add_Test (Suite, new Black.Text_IO_Tests.Test);
   Ahven.Text_Runner.Run (Suite);
end Runner;
