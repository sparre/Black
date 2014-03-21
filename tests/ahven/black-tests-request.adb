with
  Ada.Containers,
  Ada.Exceptions,
  Ada.Streams.Stream_IO,
  Ada.Strings.Unbounded;
with
  Black.HTTP,
  Black.Parameter.Vectors,
  Black.Request;

package body Black.Tests.Request is
   generic
      Source_File_Name   : String;
      Method_Used        : Black.HTTP.Methods;
      Resource_Requested : String;
      Parameters_Passed  : Parameter.Vectors.Vector;
   procedure Parser_Test;

   function Image (Item : in Parameter.Instance) return String;
   function Image (Item : in Parameter.Vectors.Vector) return String;

   function Image (Item : in Parameter.Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      if Item.With_Value then
         return
           """" & To_String (Item.Key) & """ = """ & To_String (Item.Value) &
           """";
      else
         return  """" & To_String (Item.Key) & """";
      end if;
   end Image;

   function Image (Item : in Parameter.Vectors.Vector) return String is
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;
      Buffer : Unbounded_String;
   begin
      if Item.Length = 0 then
         return "�";
      else
         Append (Buffer, "{");
         for Index in Item.First_Index .. Item.Last_Index loop
            Append (Buffer, Image (Item.Element (Index)));
            if Index = Item.Last_Index then
               Append (Buffer, "}");
            else
               Append (Buffer, ", ");
            end if;
         end loop;
         return To_String (Buffer);
      end if;
   end Image;

   procedure Parser_Test is
      use Ada.Streams.Stream_IO;
      Source : File_Type;
   begin
      Open (File => Source,
            Name => "ahven/" & Source_File_Name,
            Mode => In_File);

      declare
         use type Black.HTTP.Methods, Black.Parameter.Vectors.Vector;
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

         Ahven.Assert
           (Condition => Parameters_Passed = Got.Parameters,
            Message   => "Parser found " & Source_File_Name & " to ask for " &
                         Image (Got.Parameters) & " (expected " &
                         Image (Parameters_Passed) & ").");
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

   function "+" (Item : in String)
                return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   use type Parameter.Vectors.Vector;

   Example_3_Parameters : constant Parameter.Vectors.Vector :=
     Parameter.Instance'(Key        => +"a",
                         Value      => +"b",
                         With_Value => True) &
     Parameter.Instance'(Key        => +"c",
                         With_Value => False) &
     Parameter.Instance'(Key        => +"D",
                         Value      => +"Elefant",
                         With_Value => True);

   Example_5_Parameters : constant Parameter.Vectors.Vector :=
     Parameter.Vectors.Empty_Vector &
     Parameter.Instance'(Key        => +"a b",
                         Value      => +"c d",
                         With_Value => True);

   procedure Example_1 is
      new Parser_Test (Source_File_Name   => "example_1.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/some/resource",
                       Parameters_Passed  => Parameter.Vectors.Empty_Vector);
   procedure Example_2 is
      new Parser_Test (Source_File_Name   => "example_2.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/some/resource/",
                       Parameters_Passed  => Parameter.Vectors.Empty_Vector);
   procedure Example_3 is
      new Parser_Test (Source_File_Name   => "example_3.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/some/resource/",
                       Parameters_Passed  => Example_3_Parameters);
   procedure Example_4 is
      new Parser_Test (Source_File_Name   => "example_4.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/",
                       Parameters_Passed  => Parameter.Vectors.Empty_Vector);
   procedure Example_5 is
      new Parser_Test (Source_File_Name   => "example_5.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/blåbærgrød",
                       Parameters_Passed  => Example_5_Parameters);

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