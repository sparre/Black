with
  Ada.Characters.Handling;
with
  Black.Text_IO;

package body Black.Request is
   function Method (Request : in Instance) return HTTP.Methods is
   begin
      if Request.Blank then
         raise Constraint_Error with "Request is blank.";
      else
         return Request.Method;
      end if;
   end Method;

   procedure Parse (Request : in out Instance;
                    Line    : in     Ada.Strings.Unbounded.Unbounded_String) is
      function Key return String;
      function Value return String;

      use Ada.Strings.Unbounded;
      Split_Position : constant Natural := Index (Line, ": ");

      function Key return String is
         use Ada.Characters.Handling;
      begin
         return To_Upper (Slice (Line, 1, Split_Position - 1));
      end Key;

      function Value return String is
      begin
         return Slice (Line, Split_Position + 2, Length (Line));
      end Value;
   begin
      if Split_Position = 0 then
         raise Protocol_Error;
      else
         null; --  Not storing any data from headers at the moment.
         pragma Unreferenced (Request, Key, Value);
      end if;
   end Parse;

   function Parse_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Instance is
      use Ada.Strings.Unbounded;
      Previous_Line, Current_Line : Unbounded_String;
   begin
      return R : Instance do
         R.Parse_Method_And_Resource (Text_IO.Get_Line (Stream));

         loop
            Current_Line := Text_IO.Get_Line (Stream);

            if Length (Current_Line) = 0 then
               R.Parse (Previous_Line);
               exit;
            elsif Element (Current_Line, 1) = ' ' then
               Current_Line := Previous_Line & Current_Line;
            else
               R.Parse (Previous_Line);
            end if;

            Previous_Line := Current_Line;
         end loop;
      end return;
   end Parse_HTTP;

   procedure Parse_Method_And_Resource
     (Request : in out Instance;
      Line    : in     Ada.Strings.Unbounded.Unbounded_String) is
      use Ada.Strings.Unbounded;
      First_Space  : constant Natural := Index (Line, " ");
      Second_Space : constant Natural := Index (Line, " ", First_Space + 1);
   begin
      if Second_Space = 0 then
         raise Protocol_Error;
      else
         Parse_Method :
         declare
            Method : constant String := (Slice (Line, 1, First_Space - 1));
         begin
            Request.Method := HTTP.Methods'Value (Method);
         exception
            when Constraint_Error =>
               raise Protocol_Error
                 with """" & Method & """ is not a recognised HTTP method.";
         end Parse_Method;

         Parse_Resource :
         begin
            if Second_Space - First_Space > 1 then
               Request.Resource :=
                 Unbounded_Slice (Line, First_Space + 1, Second_Space - 1);
            else
               raise Protocol_Error
                 with "Empty resource identifier.";
            end if;
         end Parse_Resource;

         Parse_Protocol_Version :
         begin
            if Slice (Line, Second_Space + 1, Length (Line)) /= "HTTP/1.1" then
               raise Protocol_Error
                 with "HTTP 1.1 is the only supported protocol version.";
            end if;
         end Parse_Protocol_Version;

         Request.Blank := False;
      end if;
   end Parse_Method_And_Resource;

   function Resource (Request : in Instance) return String is
   begin
      if Request.Blank then
         raise Constraint_Error with "Request is blank.";
      else
         return Ada.Strings.Unbounded.To_String (Request.Resource);
      end if;
   end Resource;
end Black.Request;
