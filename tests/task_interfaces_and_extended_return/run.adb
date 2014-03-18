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

   raise Program_Error
     with "The error in GNAT seems to have been fixed now.";
exception
   when E : Storage_Error =>
      Put_Line ("Storage_Error with """ & Exception_Message (E) & """");
   when E : others =>
      Put_Line
        (Exception_Name (E) & " with """ & Exception_Message (E) & """");
      raise;
end Run;
