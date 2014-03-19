with
  Ahven.Framework,
  Ahven.Text_Runner;

procedure Black.Tests.Run is
   Suite : Ahven.Framework.Test_Suite := Tests.Suite;
begin
   Ahven.Text_Runner.Run (Suite);
end Black.Tests.Run;
