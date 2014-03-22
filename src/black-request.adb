with
  Ada.Characters.Handling,
  Ada.Strings.Fixed;
with
  URL_Utilities;
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

   function Parameters (Request : in Instance)
                       return Parameter.Vectors.Vector is
   begin
      if Request.Blank then
         raise Constraint_Error with "Request is blank.";
      else
         return Request.Parameters;
      end if;
   end Parameters;

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
         raise Protocol_Error
           with "Can not split """ & To_String (Line) & """ in key and value.";
      else
         if Key = "UPGRADE" and Value = "websocket" then
            Request.Websocket := True;
         elsif Key = "SEC-WEBSOCKET-KEY" then
            Request.Websocket_Key := To_Unbounded_String (Value);
            Request.Has_Websocket_Key := True;
         end if;
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

         Previous_Line := Text_IO.Get_Line (Stream);

         if Length (Previous_Line) > 0 then
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
         end if;
      end return;
   end Parse_HTTP;

   procedure Parse_Method_And_Resource
     (Request : in out Instance;
      Line    : in     Ada.Strings.Unbounded.Unbounded_String) is

      function Parse_Parameters (Raw : in String)
                                return Parameter.Vectors.Vector;

      function Parse_Parameters (Raw : in String)
                                return Parameter.Vectors.Vector is
         use Ada.Strings.Fixed;
         From : Positive := Raw'First;
         Next : Positive;
      begin
         return Result : Parameter.Vectors.Vector do
            loop
               exit when From > Raw'Last;

               Next := Index (Raw (From .. Raw'Last) & "&", "&");

               declare
                  use Ada.Strings.Unbounded;
                  Current : constant String := Raw (From .. Next - 1);
                  Equals  : constant Natural := Index (Current, "=");
               begin
                  if Equals in Current'Range then
                     Result.Append
                       ((Key        => To_Unbounded_String
                                         (URL_Utilities.Decode
                                           (Current
                                              (Current'First .. Equals - 1))),
                         With_Value => True,
                         Value      => To_Unbounded_String
                                         (URL_Utilities.Decode
                                           (Current
                                              (Equals + 1 .. Current'Last)))));
                  else
                     Result.Append ((Key        => To_Unbounded_String
                                                     (URL_Utilities.Decode
                                                        (Current)),
                                     With_Value => False));
                  end if;
               end;

               From := Next + 1;
            end loop;
         end return;
      end Parse_Parameters;

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

         Parse_Resource_And_Parameters :
         begin
            if Second_Space - First_Space > 1 then
               declare
                  Parameter_Marker : constant Natural :=
                    Index (Line, "?", First_Space + 1);
               begin
                  if Parameter_Marker in 1 .. Second_Space - 1 then
                     Request.Resource :=
                       To_Unbounded_String
                         (URL_Utilities.Decode
                            (Slice (Source => Line,
                                    Low    => First_Space + 1,
                                    High   => Parameter_Marker - 1)));
                     Request.Parameters :=
                       Parse_Parameters (Slice (Source => Line,
                                                Low    => Parameter_Marker + 1,
                                                High   => Second_Space - 1));
                  else
                     Request.Resource :=
                       To_Unbounded_String
                         (URL_Utilities.Decode
                            (Slice (Source => Line,
                                    Low    => First_Space + 1,
                                    High   => Second_Space - 1)));
                  end if;
               end;
            else
               raise Protocol_Error
                 with "Empty resource identifier.";
            end if;
         end Parse_Resource_And_Parameters;

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

   function Want_Websocket (Request : in Instance) return Boolean is
   begin
      if Request.Blank then
         raise Constraint_Error with "Request is blank.";
      else
         return Request.Websocket;
      end if;
   end Want_Websocket;

   function Websocket_Key  (Request : in Instance) return String is
   begin
      if Request.Blank then
         raise Constraint_Error with "Request is blank.";
      elsif not Request.Websocket then
         raise Constraint_Error with "Not a websocket request.";
      elsif Request.Has_Websocket_Key then
         return Ada.Strings.Unbounded.To_String (Request.Websocket_Key);
      else
         raise Protocol_Error with "Request has no websocket key.";
      end if;
   end Websocket_Key;
end Black.Request;
