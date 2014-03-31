with
  Ada.Tags;
with
  Black.Response,
  Black.Streams.Memory;

package body Black.Tests.Memory_Streams is
   procedure HTTP_Responses is
      use type Ada.Tags.Tag;
      use Black.Response;
      Buffer : aliased Streams.Memory.Instance;
      Input  : constant Class := OK ("We are living in a yellow ...");
      Output : Class := Not_Found ("none");
   begin
      Class'Write (Buffer'Access, Input);
      Class'Read  (Buffer'Access, Output);

      Ahven.Assert
        (Condition => Input = Output,
         Message   => "Output from in-memory stream inconsistent with input.");
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
