with
  Ada.Containers,
  Ada.Exceptions,
  Ada.Streams.Stream_IO,
  Ada.Strings.Unbounded,
  Ada.Text_IO,
  Ada.Text_IO.Text_Streams;
with
  Black.HTTP,
  Black.Parameter.Vectors,
  Black.Request,
  Black.Streams.Memory;

package body Black.Tests.Request is
   generic
      Source_File_Name   : String;
      Method_Used        : Black.HTTP.Methods;
      Resource_Requested : String;
      Parameters_Passed  : Parameter.Vectors.Vector;
      Want_Websocket     : Boolean := False;
      Websocket_Key      : String := "";
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

   procedure Output_Input_Test is
      use type Black.Request.Instance;
      Examples : constant array (Positive range <>) of Black.Request.Instance
        := (1 => Black.Request.Compose (Method   => HTTP.Get,
                                        Host     => "lego.sparre-andersen.dk",
                                        Resource => "/Transport/Biler/"),
            2 => Black.Request.Compose (Method   => HTTP.Get,
                                        Host     => "ada-dk.org",
                                        Resource => "/"),
            3 => Black.Request.Compose (Method   => HTTP.Put,
                                        Host     => "ada-dk.org",
                                        Resource => "/events"),
            4 => Black.Request.Compose (Method   => HTTP.Delete,
                                        Host     => "ada-dk.org",
                                        Resource => "/events/3"),
            5 => Black.Request.Compose (Method   => HTTP.Post,
                                        Host     => "ada-dk.org",
                                        Resource => "/events/2",
                                        Content  => "Hello guys!"),
            6 => Black.Request.Compose (Method   => HTTP.Options,
                                        Host     => "ada-dk.org",
                                        Resource => "/events"),
            7 => Black.Request.Websocket (Host     => "ada-dk.org",
                                          Resource => "/events",
                                          Key      => "dead=abba=beef"));
      Buffer   : aliased Black.Streams.Memory.Instance;
      Got      : Black.Request.Instance;
   begin
      for Index in Examples'Range loop
         Black.Request.Instance'Output (Buffer'Access,
                                        Examples (Index));
         Got := Black.Request.Instance'Input (Buffer'Access);

         declare
            use Ada.Text_IO, Ada.Text_IO.Text_Streams;
         begin
            Put_Line ("Example" & Positive'Image (Index) & ":");
            Black.Request.Instance'Output (Stream (Standard_Output),
                                           Got);
         end;

         Ahven.Assert
           (Condition => Examples (Index) = Got,
            Message   => "Mismatch in example" & Positive'Image (Index) &
                         ".  (Skipping remaining examples.)");
      end loop;
   end Output_Input_Test;

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

         if Want_Websocket then
            Ahven.Assert
              (Condition => Want_Websocket = Got.Want_Websocket,
               Message   => "Parser found " & Source_File_Name &
                            " not to ask for a websocket.");

            Ahven.Assert
              (Condition => Websocket_Key = Got.Websocket_Key,
               Message   => "Parser found " & Source_File_Name &
                            " to pass the websocket key " &
                            Got.Websocket_Key & " where " &
                            Websocket_Key & " was expected.");
         else
            Ahven.Assert
              (Condition => Want_Websocket = Got.Want_Websocket,
               Message   => "Parser found " & Source_File_Name &
                            " to ask for a websocket.");
         end if;
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
   procedure Example_6 is
      new Parser_Test (Source_File_Name   => "example_6.HTTP-request",
                       Method_Used        => HTTP.Get,
                       Resource_Requested => "/chat",
                       Parameters_Passed  => Parameter.Vectors.Empty_Vector,
                       Want_Websocket     => True,
                       Websocket_Key      => "dGhlIHNhbXBsZSBub25jZQ==");
   procedure Example_7 is
      new Parser_Test (Source_File_Name   => "example_7.HTTP-request",
                       Method_Used        => HTTP.Post,
                       Resource_Requested => "/",
                       Parameters_Passed  => Parameter.Vectors.Empty_Vector);

   pragma Style_Checks (Off);
   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("HTTP Requests");

      Add_Test_Routine (T, Example_1'Access,         "Request parser (example 1 - get)");
      Add_Test_Routine (T, Example_2'Access,         "Request parser (example 2 - get)");
      Add_Test_Routine (T, Example_3'Access,         "Request parser (example 3 - get)");
      Add_Test_Routine (T, Example_4'Access,         "Request parser (example 4 - get)");
      Add_Test_Routine (T, Example_5'Access,         "Request parser (example 5 - get)");
      Add_Test_Routine (T, Example_6'Access,         "Request parser (example 6 - websocket)");
      Add_Test_Routine (T, Example_7'Access,         "Request parser (example 7 - post)");
      Add_Test_Routine (T, Output_Input_Test'Access, "Output->input filter");
   end Initialize;
end Black.Tests.Request;
