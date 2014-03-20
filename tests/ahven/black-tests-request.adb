with
  Ada.Exceptions,
  Ada.Streams.Stream_IO;
with
  Black.HTTP,
  Black.Request;

package body Black.Tests.Request is
   generic
      Source_File_Name   : String;
      Method_Used        : Black.HTTP.Methods;
      Resource_Requested : String;
   procedure Parser_Test;

   procedure Parser_Test is
      use Ada.Streams.Stream_IO;
      Source : File_Type;
   begin
      Open (File => Source,
            Name => "ahven/" & Source_File_Name,
            Mode => In_File);

      declare
         use type Black.HTTP.Methods;
         Got : constant Black.Request.Instance :=
                 Black.Request.Instance'Input (Stream (Source));
      begin
         Ahven.Assert
           (Condition => Method_Used = Got.Method,
            Message   => "Parser found " & Source_File_Name & " to use " &
                         Black.HTTP.Methods'Image (Got.Method) & ".");

         Ahven.Assert
           (Condition => Resource_Requested = Got.Resource,
            Message   => "Parser found " & Source_File_Name & " to ask for " &
                         Got.Resource & " (expected " & Resource_Requested &
                         ").");
      end;

      Close (File => Source);
   exception
      when E : others =>
         declare
            use Ada.Exceptions;
         begin
            Ahven.Fail
              (Message => "Exception " & Exception_Name (E) &
                          " with message """ & Exception_Message (E) &
                          """.");
         end;
   end Parser_Test;

   procedure Example_1 is
      new Parser_Test (Source_File_Name   => "example_1.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/some/resource");
   procedure Example_2 is
      new Parser_Test (Source_File_Name   => "example_2.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/some/resource/");
   procedure Example_3 is
      new Parser_Test (Source_File_Name   => "example_3.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/some/resource/");
   procedure Example_4 is
      new Parser_Test (Source_File_Name   => "example_4.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/");
   procedure Example_5 is
      new Parser_Test (Source_File_Name   => "example_5.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/blåbærgrød");

   pragma Style_Checks (Off);
   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("HTTP Requests");

      Add_Test_Routine (T, Example_1'Access, "Request parser (example 1)");
      Add_Test_Routine (T, Example_2'Access, "Request parser (example 2)");
      Add_Test_Routine (T, Example_3'Access, "Request parser (example 3)");
      Add_Test_Routine (T, Example_4'Access, "Request parser (example 4)");
      Add_Test_Routine (T, Example_5'Access, "Request parser (example 5)");
   end Initialize;
end Black.Tests.Request;
