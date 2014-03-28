with
  System.Storage_Elements;
with
  GNAT.SHA1;
with
  Black.Text_IO;

package body Black.Response is
   function Not_Found (Resource : in String) return Class is
      use Text_IO;
      Message : constant String := "The requested resource, '" & Resource &
                                   "' was not found on the server.";
   begin
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 404 Not Found");
         Put_Line (R.Data, "Content-Type: text/plain; charset=iso-8859-1");
         Put_Line (R.Data, "Content-Length:" & Natural'Image (Message'Length));
         New_Line (R.Data);
         Put      (R.Data, Message);

         R.Complete := True;
      end return;
   end Not_Found;

   function OK (Content_Type : in String;
                Data         : in Ada.Streams.Stream_Element_Array)
               return Class is
      use Text_IO;
   begin
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 200 OK");
         Put_Line (R.Data, "Content-Type: " & Content_Type);
         Put_Line (R.Data, "Content-Length:" & Natural'Image (Data'Length));
         New_Line (R.Data);
         R.Data.Append (Data);

         R.Complete := True;
      end return;
   end OK;

   function OK (Data : in String)
               return Class is
      use Text_IO;
   begin
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 200 OK");
         Put_Line (R.Data, "Content-Type: text/plain; charset=iso-8859-1");
         Put_Line (R.Data, "Content-Length:" & Natural'Image (Data'Length));
         New_Line (R.Data);
         Put      (R.Data, Data);

         R.Complete := True;
      end return;
   end OK;

   procedure Output_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Instance) is
      use Ada.Streams;
   begin
      if Item.Complete then
         for Index in Item.Data.First_Index .. Item.Data.Last_Index loop
            Stream.Write (Item.Data.Element (Index));
         end loop;
      else
         raise Constraint_Error
           with "Response object is not ready to be streamed.";
      end if;
   end Output_HTTP;

   function Redirect (Target    : in String;
                      Permanent : in Boolean)
                     return Class is
      use Text_IO;
   begin
      return R : Instance do
         if Permanent then
            Put_Line (R.Data, "HTTP/1.1 301 Moved Permanently");
         else
            Put_Line (R.Data, "HTTP/1.1 302 Moved Temporarily");
         end if;

         Put_Line (R.Data, "Location: " & Target);
         Put_Line (R.Data, "Content-Length: 0");
         New_Line (R.Data);

         R.Complete := True;
      end return;
   end Redirect;

   function Switch_To_Websocket (Key : in String) return Class is
      function Accept_Key return String;
      function To_Storage_Elements
                 (Hex : in String)
                 return System.Storage_Elements.Storage_Array;
      function Base64 (Octets : in System.Storage_Elements.Storage_Array)
                      return String;

      function Accept_Key return String is
         use GNAT.SHA1;
         Hash : Context := Initial_Context;
      begin
         Update (Hash, Key);
         Update (Hash, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11");
         return Base64 (To_Storage_Elements (Digest (Hash)));
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
                                 Hex'First + Positive (2 * Index);
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
      return R : Instance do
         Put_Line (R.Data, "HTTP/1.1 101 Switching Protocols");
         Put_Line (R.Data, "Upgrade: websocket");
         Put_Line (R.Data, "Connection: Upgrade");
         Put_Line (R.Data, "Sec-WebSocket-Accept: " & Accept_Key);
         New_Line (R.Data);

         R.Complete := True;
      end return;
   end Switch_To_Websocket;
end Black.Response;
