with
  Ada.Exceptions,
  Ada.Text_IO;
with
  Task_Interface;
procedure Run is
   use Ada.Exceptions, Ada.Text_IO;
begin
   Put_Line ("One not declared yet.");
   declare
      One : Task_Interface.Class := Task_Interface.Create (1);
   begin
      Put_Line ("One declared.");
      One.Start;
      Put_Line ("One started.");
   end;
exception
   when E : others =>
      Put_Line ("Message: """ & Exception_Message (E));
      raise;
end Run;
