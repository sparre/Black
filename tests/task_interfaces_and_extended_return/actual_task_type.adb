with
  Ada.Integer_Text_IO,
  Ada.Text_IO;
package body Actual_Task_Type is
   task body Instance is
      use Ada.Integer_Text_IO, Ada.Text_IO;
   begin
      Put (ID); Put_Line (" waiting to start...");
      accept Start;
      Put (ID); Put_Line ("started.");
      delay 1.0;
      Put (ID); Put_Line ("stopping.");
   end Instance;
end Actual_Task_Type;
