with
  Ada.Strings.Fixed;
with
  URL_Utilities;
with
  Black.Text_IO;

package body Black.Request is

   procedure Check (Item : in     Instance);
   procedure Check (Content        : in     Optional_String.Instance;
                    Content_Type   : in     Optional_String.Instance;
                    Content_Length : in     Optional_Natural.Instance);

   ----------------------------------------------------------------------------

   procedure Check (Content        : in     Optional_String.Instance;
                    Content_Type   : in     Optional_String.Instance;
                    Content_Length : in     Optional_Natural.Instance) is
      use Ada.Strings.Unbounded;
   begin
      if Content.Set and not Content_Type.Set then
         raise Constraint_Error
           with "Request is not ready for transmission: " &
                "No MIME type declared for content.";
      elsif Content.Set and Content_Length.Set then
         if Content_Length.Value /= Length (Content.Value) then
            raise Constraint_Error
              with "Request is not ready for transmission: " &
                   "Declared content length does not match actual content " &
                   "length.";
         end if;
      elsif Content_Length.Set and then Content_Length.Value > 0 then
         pragma Assert (not Content.Set);
         raise Constraint_Error
           with "Request is not ready for transmission: " &
                "Positive content length, but no content provided.";
      end if;
   end Check;

   procedure Check (Item : in     Instance) is
   begin
      if not Item.Method.Set then
         raise Constraint_Error
           with "Request does not have a HTTP method.";
      elsif not Item.Host.Set then
         raise Constraint_Error
           with "Request does not contain a host name.";
      elsif Item.Websocket and not Item.Websocket_Key.Set then
         raise Constraint_Error
           with "Request is not ready for transmission: No websocket key.";
      end if;

      Check (Content        => Item.Content,
             Content_Type   => Item.Content_Type,
             Content_Length => Item.Content_Length);
   end Check;

   function Compose (Method   : in HTTP.Methods;
                     Host     : in String;
                     Resource : in String) return Instance is
      use Ada.Strings.Unbounded;
      use type Black.Optional_HTTP_Method.Instance;
      use type Black.Optional_String.Instance;
   begin
      return (Method         => +Method,
              Host           => +To_Unbounded_String (Host),
              Resource       => +To_Unbounded_String (Resource),
              Parameters     => <>,
              Websocket      => False,
              Websocket_Key  => <>,
              Origin         => <>,
              Content        => <>,
              Content_Type   => <>,
              Content_Length => <>);
   end Compose;

   function Compose (Method       : in HTTP.Methods;
                     Host         : in String;
                     Resource     : in String;
                     Content      : in String;
                     Content_Type : in String := MIME_Types.Text.Plain)
                    return Instance is
      use Ada.Strings.Unbounded;
      use type Black.Optional_HTTP_Method.Instance;
      use type Black.Optional_Natural.Instance;
      use type Black.Optional_String.Instance;
   begin
      return (Method             => +Method,
              Host               => +To_Unbounded_String (Host),
              Resource           => +To_Unbounded_String (Resource),
              Parameters         => <>,
              Websocket          => False,
              Websocket_Key      => <>,
              Origin             => <>,
              Content            => +To_Unbounded_String (Content),
              Content_Type       => +To_Unbounded_String (Content_Type),
              Content_Length     => +Content'Length);
   end Compose;

   procedure Generate_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Instance) is
      use Ada.Strings.Unbounded;
      use Text_IO;
   begin
      Check (Item);

      Put      (Stream, HTTP.Methods'Image (Item.Method.Value));
      Put      (Stream, " ");
      Put      (Stream, Item.Resource.Value);
      Put      (Stream, " ");
      Put_Line (Stream, HTTP.Version);

      Put      (Stream, "Host: ");
      Put_Line (Stream, Item.Host.Value);

      Put_Line (Stream, "Accept-Encoding: identity");

      if Item.Websocket then
         Put_Line (Stream, "Upgrade: websocket");
         Put_Line (Stream, "Connection: Upgrade");

         Put      (Stream, "Sec-Websocket-Key: ");
         Put_Line (Stream, Item.Websocket_Key.Value);
      end if;

      if Item.Has_Origin then
         Put      (Stream, "Origin: ");
         Put_Line (Stream, Item.Origin.Value);
      end if;

      if Item.Content.Set then
         Put      (Stream, "Content-Type: ");
         Put_Line (Stream, Item.Content_Type.Value);
         Put      (Stream, "Content-Length: ");
         Put_Line (Stream, Length (Item.Content.Value));
      end if;

      New_Line (Stream);

      if Item.Content.Set then
         Put      (Stream, Item.Content.Value);
      end if;
   end Generate_HTTP;

   function Has_Origin (Request : in Instance) return Boolean is
   begin
      return Request.Origin.Set;
   end Has_Origin;

   function Has_Parameter (Request : in Instance;
                           Key     : in String) return Boolean is
      use Ada.Strings.Unbounded;
      use Black.Parameter.Vectors;
   begin
      for Index in Request.Parameters.First_Index ..
                   Request.Parameters.Last_Index loop
         if To_String (Request.Parameters.Element (Index).Key) = Key then
            return True;
         end if;
      end loop;

      return False;
   end Has_Parameter;

   function Host (Request : in Instance) return String is
      use Ada.Strings.Unbounded;
   begin
      if Request.Host.Set then
         return To_String (Request.Host.Value);
      else
         raise Protocol_Error with "No host name was provided.";
      end if;
   end Host;

   function Method (Request : in Instance) return HTTP.Methods is
   begin
      if Request.Method.Set then
         return Request.Method.Value;
      else
         raise Constraint_Error with "Request is blank.";
      end if;
   end Method;

   function Origin (Request : in Instance) return String is
   begin
      if Request.Origin.Set then
         return Ada.Strings.Unbounded.To_String (Request.Origin.Value);
      else
         raise Constraint_Error with "Request has no origin.";
      end if;
   end Origin;

   function Parameter (Request : in Instance;
                       Key     : in String;
                       Default : in String) return String is
      use Black.Parameter;
   begin
      return Request.Parameter (Key => Key);
   exception
      when No_Such_Parameter_Key | No_Such_Parameter_Value =>
         return Default;
   end Parameter;

   function Parameter (Request : in Instance;
                       Key     : in String) return String is
      use Black.Parameter;
      use Black.Parameter.Vectors;
   begin
      if not Request.Has_Parameter (Key => Key) then
         raise No_Such_Parameter_Key;
      end if;

      for Index in Request.Parameters.First_Index ..
                   Request.Parameters.Last_Index loop
         declare
            P : Black.Parameter.Instance renames
                  Request.Parameters.Element (Index);
         begin
            if Ada.Strings.Unbounded.To_String (P.Key) = Key then
               return Ada.Strings.Unbounded.To_String (P.Value);
            end if;
         end;
      end loop;

      raise No_Such_Parameter_Value;
   end Parameter;

   function Parameters (Request : in Instance)
                       return Black.Parameter.Vectors.Vector is
   begin
      return Request.Parameters;
   end Parameters;

   procedure Parse (Request : in out Instance;
                    Line    : in     Black.Parsing.Header_Line) is
      use type HTTP.Header_Key;
      use type Optional_Natural.Instance;
      use type Optional_String.Instance;
   begin
      if Line.Key = "Host" then
         Request.Host := +Line.Value;
      elsif Line.Key = "Upgrade" and Line.Value = "websocket" then
         Request.Websocket := True;
      elsif Line.Key = "Sec-Websocket-Key" then
         Request.Websocket_Key := +Line.Value;
      elsif Line.Key = "Origin" then
         Request.Origin := +Line.Value;
      elsif Line.Key = "Content-Type" then
         Request.Content_Type := +Line.Value;
      elsif Line.Key = "Content-Length" then
         Request.Content_Length := +Line.Value;
      end if;
   end Parse;

   function Parse_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Instance is
      use Ada.Strings.Unbounded;
      use type Black.HTTP.Methods;
      Header : Black.Parsing.Header;
      Line   : Black.Parsing.Header_Line;
   begin
      return R : Instance do
         R.Parse_Method_And_Resource (Text_IO.Get_Line (Stream));

         Header := Black.Parsing.Get (Stream);
         while not Black.Parsing.End_Of_Header (Header) loop
            Black.Parsing.Read (Stream => Stream,
                                From   => Header,
                                Item   => Line);
            R.Parse (Line);
         end loop;

         if R.Content_Length.Set and then R.Content_Length.Value > 0 then
            declare
               use type Optional_String.Instance;
               Buffer : String (1 .. R.Content_Length.Value);
            begin
               String'Read (Stream,
                            Buffer);
               R.Content := +To_Unbounded_String (Buffer);
            end;

            if not R.Content_Type.Set then
               raise Protocol_Error
                 with "No content type provided with content.";
            end if;
         elsif R.Content_Type.Set then
            raise Program_Error
              with "Parsing of request content without a content length not " &
                   "implemented.";
         elsif R.Method.Value = HTTP.Post then
            raise Protocol_Error
              with "A POST request is required to have a content type.";
         end if;
      end return;
   end Parse_HTTP;

   procedure Parse_Method_And_Resource
     (Request : in out Instance;
      Line    : in     Ada.Strings.Unbounded.Unbounded_String) is

      function Parse_Parameters (Raw : in String)
                                return Black.Parameter.Vectors.Vector;

      function Parse_Parameters (Raw : in String)
                                return Black.Parameter.Vectors.Vector is
         use Ada.Strings.Fixed;
         From : Positive := Raw'First;
         Next : Positive;
      begin
         return Result : Black.Parameter.Vectors.Vector do
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
         raise Protocol_Error
           with """" & To_String (Line) & """ only contains a single space. " &
                "Expected format: <method> <resource> HTTP/1.1";
      else
         Parse_Method :
         declare
            use type Optional_HTTP_Method.Instance;
            Method : constant String := (Slice (Line, 1, First_Space - 1));
         begin
            Request.Method := +HTTP.Methods'Value (Method);
         exception
            when Constraint_Error =>
               raise Protocol_Error
                 with """" & Method & """ is not a recognised HTTP method.";
         end Parse_Method;

         Parse_Resource_And_Parameters :
         begin
            if Second_Space - First_Space > 1 then
               declare
                  use type Optional_String.Instance;
                  Parameter_Marker : constant Natural :=
                    Index (Line, "?", First_Space + 1);
               begin
                  if Parameter_Marker in 1 .. Second_Space - 1 then
                     Request.Resource :=
                       +To_Unbounded_String
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
                       +To_Unbounded_String
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
                 with "HTTP 1.1 is the only supported protocol version.  (" &
                      To_String (Line) & ")";
            end if;
         end Parse_Protocol_Version;
      end if;
   end Parse_Method_And_Resource;

   function Resource (Request : in Instance) return String is
   begin
      if Request.Resource.Set then
         return Ada.Strings.Unbounded.To_String (Request.Resource.Value);
      else
         raise Constraint_Error with "Request has no resource.";
      end if;
   end Resource;

   function Want_Websocket (Request : in Instance) return Boolean is
   begin
      return Request.Websocket;
   end Want_Websocket;

   function Websocket (Host     : in String;
                       Resource : in String;
                       Key      : in String) return Instance is
      use Ada.Strings.Unbounded;
      use type Black.Optional_HTTP_Method.Instance;
      use type Black.Optional_String.Instance;
   begin
      return (Method         => +HTTP.Get,
              Host           => +To_Unbounded_String (Host),
              Resource       => +To_Unbounded_String (Resource),
              Parameters     => <>,
              Websocket      => True,
              Websocket_Key  => +To_Unbounded_String (Key),
              Origin         => <>,
              Content        => <>,
              Content_Type   => <>,
              Content_Length => <>);
   end Websocket;

   function Websocket_Key  (Request : in Instance) return String is
   begin
      if not Request.Websocket then
         raise Constraint_Error with "Not a websocket request.";
      elsif Request.Websocket_Key.Set then
         return Ada.Strings.Unbounded.To_String (Request.Websocket_Key.Value);
      else
         raise Protocol_Error with "Request has no websocket key.";
      end if;
   end Websocket_Key;
end Black.Request;
