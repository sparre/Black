with
  Ada.Strings.Unbounded;
with
  Black.Response,
  Black.Streams.Memory,
  Black.Text_IO;

package body Black.Tests.Memory_Streams is
   procedure HTTP_Responses is
      use Ada.Strings.Unbounded;
      use Black.Response;

      function "+" (Item : in String) return Unbounded_String
        renames To_Unbounded_String;

      Buffer   : aliased Streams.Memory.Instance;
      Message  : constant String := "We are living in a yellow ...";
      Input    : constant Class  := OK (Data => Message);
      Expected : constant array (1 .. 5) of Unbounded_String :=
        (+"HTTP/1.1 200 OK",
         +"Content-Length:" & Natural'Image (Message'Length),
         +"Content-Type: text/plain; charset=iso-8859-1",
         +"",
         +Message);
   begin
      Class'Output (Buffer'Access, Input);

      for Line in Expected'Range loop
         declare
            Output : constant String := Text_IO.Get_Line (Buffer'Access);
         begin
            Ahven.Assert
              (Condition => Expected (Line) = +Output,
               Message   => "Got """ & Output & """ when """ &
                            To_String (Expected (Line)) &
                            """ was expected.");
         end;
      end loop;
   end HTTP_Responses;

   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("In-memory streams");

      Add_Test_Routine (T, Storing_Strings'Access, "Strings");
      Add_Test_Routine (T, HTTP_Responses'Access,  "HTTP responses");
   end Initialize;

   procedure Storing_Strings is
      Buffer : aliased Streams.Memory.Instance;
      Input  : constant String := "We are living in a yellow ...";
   begin
      String'Output (Buffer'Access, Input);

      declare
         Output : constant String := String'Input (Buffer'Access);
      begin
         Ahven.Assert
           (Condition => Input = Output,
            Message   => """" & Input & """ /= """ & Output & """");
      end;
   end Storing_Strings;
end Black.Tests.Memory_Streams;
