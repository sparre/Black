with
  Ada.IO_Exceptions,
  Ada.Strings.Fixed;
with
  System.Storage_Elements;
with
  GNAT.SHA1;
with
  Black.Parsing,
  Black.Text_IO;

package body Black.Response is
   function Parse (Item : in String) return Access_Control.HTTP_Status_Set;

   procedure Put_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Access_Control.HTTP_Status_Set);
   procedure Put_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Access_Controls);

   function Input_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Instance is
      use Ada.Strings.Unbounded;
      use Text_IO;
      use type HTTP.Header_Key;
      Status             : HTTP.Statuses;
      Header             : Parsing.Header;
      Line               : Parsing.Header_Line;
      Got_Content_Length : Boolean := False;
      Content_Length     : Natural := 0;
   begin
      Status := Parsing.Parse (Get_Line (Stream));
      Header := Parsing.Get (Stream);

      return R : Instance (Status) do
         while not Parsing.End_Of_Header (Header) loop
            Parsing.Read (Stream => Stream,
                          From   => Header,
                          Item   => Line);

            if Line.Key = "Content-Type" then
               R.Content_Type := Line.Value;
            elsif Line.Key = "Content-Length" then
               Got_Content_Length := True;
               Content_Length     := Line.Value;
            elsif Line.Key = "Location" then
               R.Location := Line.Value;
            elsif Line.Key = "Sec-Websocket-Accept" then
               R.Websocket_Accept :=
                 HTTP.Websocket_Accept_Key (String'(Line.Value));
            elsif Line.Key = "Access-Control-Allow-Origin" then
               R.Access_Control.Allow_Origin := Line.Value;
            elsif Line.Key = "Access-Control-Allow-Credentials" then
               R.Access_Control.Allow_Credentials := (Set   => True,
                                                      Value => Line.Value);
            elsif Line.Key = "Access-Control-Allow-Headers" then
               R.Access_Control.Allow_Headers := Parse (Line.Value);
            elsif Line.Key = "Access-Control-Max-Age" then
               R.Access_Control.Max_Age := (Set   => True,
                                            Value => Line.Value);
            end if;
         end loop;

         if Got_Content_Length then
            declare
               subtype Content is String (1 .. Content_Length);
               Buffer : Content;
            begin
               Content'Read (Stream, Buffer);
               R.Content := To_Unbounded_String (Buffer);
            end;
         else
            declare
               Buffer : Character;
            begin
               loop
                  Character'Read (Stream, Buffer);
                  Append (R.Content, Buffer);
               end loop;
            exception
               when Ada.IO_Exceptions.End_Error =>
                  null;
            end;
         end if;
      end return;
   end Input_HTTP;

   function Not_Found (Resource : in String) return Class is
      use Ada.Strings.Unbounded;
      Message : constant String := "The requested resource, '" & Resource &
                                   "' was not found on the server.";
   begin
      return Instance'(Status         => HTTP.Not_Found,
                       Content_Type   => To_Unbounded_String
                                           ("text/plain; charset=iso-8859-1"),
                       Content        => To_Unbounded_String (Message),
                       Access_Control => <>);
   end Not_Found;

   function OK (Content_Type : in String;
                Data         : in Ada.Streams.Stream_Element_Array)
               return Class is
      use Ada.Strings.Unbounded;
      pragma Assert (Ada.Streams.Stream_Element'Size = Character'Size);
      Buffer : String (1 .. Data'Length);
      for Buffer'Address use Data'Address;
   begin
      return Instance'(Status         => HTTP.OK,
                       Content_Type   => To_Unbounded_String (Content_Type),
                       Content        => To_Unbounded_String (Buffer),
                       Access_Control => <>);
   end OK;

   function OK (Data : in String) return Class is
      use Ada.Strings.Unbounded;
   begin
      return Instance'(Status         => HTTP.OK,
                       Content_Type   => To_Unbounded_String
                                           ("text/plain; charset=iso-8859-1"),
                       Content        => To_Unbounded_String (Data),
                       Access_Control => <>);
   end OK;

   procedure Output
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Class) is
   begin
      Output_HTTP (Stream, Item);
   end Output;

   procedure Output_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Instance) is
      use Ada.Strings.Unbounded;
      use type HTTP.Statuses;
   begin
      if Length (Item.Content) > 0 and Length (Item.Content_Type) = 0 then
         raise Constraint_Error
           with "Response object is not ready to be streamed.  " &
                "No content-type provided for content.";
      elsif Item.Status = HTTP.Moved_Permanently and then
              Length (Item.Location) = 0 then
         raise Constraint_Error
           with "Response object is not ready to be streamed.  " &
                "No location provided for redirection.";
      elsif Item.Status = HTTP.Moved_Temporarily and then
              Length (Item.Location) = 0 then
         raise Constraint_Error
           with "Response object is not ready to be streamed.  " &
                "No location provided for redirection.";
      else
         declare
            use Text_IO;
         begin
            Put_Line (Stream, HTTP.Status_Line (Item.Status));

            Put      (Stream, "Content-Length: ");
            Put_Line (Stream, Length (Item.Content));

            if Length (Item.Content) > 0 then
               Put      (Stream, "Content-Type: ");
               Put_Line (Stream, Item.Content_Type);
            end if;

            case Item.Status is
               when HTTP.Switching_Protocols =>
                  Put_Line (Stream, "Upgrade: websocket");
                  Put_Line (Stream, "Connection: Upgrade");
                  Put      (Stream, "Sec-WebSocket-Accept: ");
                  Put_Line (Stream, String (Item.Websocket_Accept));
               when HTTP.Moved_Permanently | HTTP.Moved_Temporarily =>
                  Put      (Stream, "Location: ");
                  Put_Line (Stream, Item.Location);
               when others =>
                  null;
            end case;

            Put_Line (Stream, Item.Access_Control);

            New_Line (Stream);
            Put      (Stream, Item.Content);
         end;
      end if;
   end Output_HTTP;

   function Parse (Item : in String) return Access_Control.HTTP_Status_Set is
      use Ada.Strings, Ada.Strings.Fixed;
      From : Integer;
      To   : Natural := Item'First - 1;
   begin
      return Result : Access_Control.HTTP_Status_Set := (others => False) do
         loop
            From := To + 1;
            exit when not (From in Item'Range);

            To := Index (Item (From .. Item'Last), ",");
            if To = 0 then
               To := Item'Last + 1;
            end if;

            declare
               Name   : String renames Trim (Item (From .. To - 1), Both);
               Status : HTTP.Statuses renames HTTP.Statuses'Value (Name);
            begin
               Result (Status) := True;
            end;
         end loop;
      end return;
   exception
      when others =>
         raise Protocol_Error
           with "Could not parse Access-Control-Allow-Methods value.";
   end Parse;

   procedure Put_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Access_Control.HTTP_Status_Set) is
      First : Boolean := True;
   begin
      for Status in Item'Range loop
         if Item (Status) then
            if not First then
               Text_IO.Put (Target => Stream,
                            Item   => ", ");
            end if;
            Text_IO.Put (Target => Stream,
                         Item   => HTTP.Statuses'Image (Status));
            First := False;
         end if;
      end loop;
      Text_IO.New_Line (Target => Stream);
   end Put_Line;

   procedure Put_Line
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Access_Controls) is
      use Ada.Strings.Unbounded;
      use Text_IO;
      No_Headers : constant Access_Control.HTTP_Status_Set :=
                     (others => False);
      use type Access_Control.HTTP_Status_Set;
   begin
      if Length (Item.Allow_Origin) > 0 then
         Put      (Stream, "Access-Control-Allow-Origin: ");
         Put_Line (Stream, Item.Allow_Origin);
      end if;

      if Item.Allow_Credentials.Set then
         Put      (Stream, "Access-Control-Allow-Credentials: ");
         Put_Line (Stream, Item.Allow_Credentials.Value);
      end if;

      if Item.Allow_Headers /= No_Headers then
         Put      (Stream, "Access-Control-Allow-Headers: ");
         Put_Line (Stream, Item.Allow_Headers);
      end if;

      if Item.Max_Age.Set then
         Put      (Stream, "Access-Control-Max-Age: ");
         Put_Line (Stream, Item.Max_Age.Value);
      end if;

   end Put_Line;

   function Redirect (Target    : in String;
                      Permanent : in Boolean)
                     return Class is
      use Ada.Strings.Unbounded;
   begin
      if Permanent then
         return Instance'(Status         => HTTP.Moved_Permanently,
                          Content_Type   => <>,
                          Content        => <>,
                          Access_Control => <>,
                          Location       => To_Unbounded_String (Target));
      else
         return Instance'(Status         => HTTP.Moved_Temporarily,
                          Content_Type   => <>,
                          Content        => <>,
                          Access_Control => <>,
                          Location       => To_Unbounded_String (Target));
      end if;
   end Redirect;

   function Switch_To_Websocket (Key : in String) return Class is
      function Accept_Key return HTTP.Websocket_Accept_Key;
      function To_Storage_Elements
                 (Hex : in String)
                 return System.Storage_Elements.Storage_Array;
      function Base64 (Octets : in System.Storage_Elements.Storage_Array)
                      return String;

      function Accept_Key return HTTP.Websocket_Accept_Key is
         use GNAT.SHA1;
         Hash : Context := Initial_Context;
      begin
         Update (Hash, Key);
         Update (Hash, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
         return HTTP.Websocket_Accept_Key
                  (Base64 (To_Storage_Elements (Digest (Hash))));
      end Accept_Key;

      function Base64 (Octets : in System.Storage_Elements.Storage_Array)
                      return String is
         use System.Storage_Elements;
         subtype Sextet is Storage_Element range 0 .. 2 ** 6 - 1;
         Alphabet : constant array (Sextet) of Character :=
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                      "abcdefghijklmnopqrstuvwxyz" &
                      "0123456789+/";
         O_Pos  : Storage_Offset := Octets'First;
         Result : String (1 .. (Octets'Length + 2) / 3 * 4) := (others => '_');
         R_Pos  : Positive := Result'First;
      begin
         while O_Pos in Octets'Range loop
            declare
               A, B, C, D : Sextet;
            begin
               First_Byte :
               declare
                  First : Storage_Element renames Octets (O_Pos);
               begin
                  A := (First and 2#1111_1100#) / 2 ** 2;
                  B := (First and 2#0000_0011#) * 2 ** 4;

                  if O_Pos = Octets'Last then
                     Result (R_Pos .. R_Pos + 3) :=
                       Alphabet (A) & Alphabet (B) & "==";
                     return Result;
                  end if;
               end First_Byte;

               Second_Byte :
               declare
                  Second : Storage_Element renames Octets (O_Pos + 1);
               begin
                  B := B or (Second and 2#1111_0000#) / 2 ** 4;
                  C :=      (Second and 2#0000_1111#) * 2 ** 2;

                  if O_Pos + 1 = Octets'Last then
                     Result (R_Pos .. R_Pos + 3) :=
                       Alphabet (A) & Alphabet (B) & Alphabet (C) & "=";
                     return Result;
                  end if;
               end Second_Byte;

               Third_Byte :
               declare
                  Third : Storage_Element renames Octets (O_Pos + 2);
               begin
                  C := C or (Third and 2#1100_0000#) / 2 ** 6;
                  D :=      (Third and 2#0011_1111#);
               end Third_Byte;

               Result (R_Pos .. R_Pos + 3) :=
                 Alphabet (A) & Alphabet (B) & Alphabet (C) & Alphabet (D);

               O_Pos := O_Pos + 3;
               R_Pos := R_Pos + 4;
            end;
         end loop;

         return Result;
      end Base64;

      function To_Storage_Elements
                 (Hex : in String)
                 return System.Storage_Elements.Storage_Array is
         use System.Storage_Elements;
      begin
         pragma Assert (Hex'Length mod 2 = 0, "We only process octets.");

         return Octets : Storage_Array (0 .. Hex'Length / 2 - 1) do
            for Index in Octets'Range loop
               declare
                  Hex_Offset : constant Positive :=
                                 Hex'First + Positive'Base (2 * Index);
               begin
                  Octets (Index) :=
                    Storage_Element'Value
                      ("16#" & Hex (Hex_Offset .. Hex_Offset + 1) & "#");
               end;
            end loop;
         end return;
      end To_Storage_Elements;

      use Text_IO;
   begin
      return Instance'(Status           => HTTP.Switching_Protocols,
                       Content_Type     => <>,
                       Content          => <>,
                       Access_Control   => <>,
                       Websocket_Accept => Accept_Key);
   end Switch_To_Websocket;

   package body Access_Control is
      procedure Allow_Credentials (Item : in out Class) is
      begin
         Item.Access_Control.Allow_Credentials := (Set   => True,
                                                   Value => True);
      end Allow_Credentials;

      procedure Allow_Headers (Item    : in out Class;
                               Headers : in     HTTP_Status_Set) is
      begin
         Item.Access_Control.Allow_Headers := Headers;
      end Allow_Headers;

      procedure Allow_Origin (Item    : in out Class;
                              Pattern : in     String) is
         use Ada.Strings.Unbounded;
      begin
         Item.Access_Control.Allow_Origin := To_Unbounded_String (Pattern);
      end Allow_Origin;

      procedure Max_Age (Item : in out Class;
                         Age  : in     Duration) is
      begin
         Item.Access_Control.Max_Age := (Set   => True,
                                         Value => Age);
      end Max_Age;
   end Access_Control;
end Black.Response;
