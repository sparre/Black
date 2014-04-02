with
  System.Storage_Elements;
with
  GNAT.SHA1;
with
  Black.Constants,
  Black.Text_IO;

package body Black.Response is
   function Not_Found (Resource : in String) return Class is
      use Text_IO;
      Message : constant String := "The requested resource, '" & Resource &
                                   "' was not found on the server.";
   begin
      return R : Instance do
         Put_Line (R.Data'Access, Constants.Not_Found);
         Put_Line (R.Data'Access, "Content-Type: text/plain; " &
                                    "charset=iso-8859-1");
         Put_Line (R.Data'Access, "Content-Length:" & Natural'Image
                                                        (Message'Length));
         New_Line (R.Data'Access);
         Put      (R.Data'Access, Message);

         R.Complete := True;
      end return;
   end Not_Found;

   function OK (Content_Type : in String;
                Data         : in Ada.Streams.Stream_Element_Array)
               return Class is
      use Text_IO;
   begin
      return R : Instance do
         Put_Line (R.Data'Access, Constants.OK);
         Put_Line (R.Data'Access, "Content-Type: " & Content_Type);
         Put_Line (R.Data'Access, "Content-Length:" & Natural'Image
                                                        (Data'Length));
         New_Line (R.Data'Access);
         R.Data.Write (Data);

         R.Complete := True;
      end return;
   end OK;

   function OK (Data : in String)
               return Class is
      use Text_IO;
   begin
      return R : Instance do
         Put_Line (R.Data'Access, Constants.OK);
         Put_Line (R.Data'Access, "Content-Type: text/plain; " &
                                    "charset=iso-8859-1");
         Put_Line (R.Data'Access, "Content-Length:" & Natural'Image
                                                        (Data'Length));
         New_Line (R.Data'Access);
         Put      (R.Data'Access, Data);

         R.Complete := True;
      end return;
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
   begin
      if Item.Complete then
         declare
            use Ada.Streams;
            Data   : Streams.Memory.Instance := Item.Data.Copy;
            Buffer : Stream_Element_Array (1 .. 10_000);
            Last   : Stream_Element_Offset;
         begin
            loop
               Data.Read (Buffer, Last);
               Stream.Write (Buffer (Buffer'First .. Last));
               exit when Last < Buffer'Last;
            end loop;
         end;
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
            Put_Line (R.Data'Access, Constants.Moved_Permanently);
         else
            Put_Line (R.Data'Access, Constants.Moved_Temporarily);
         end if;

         Put_Line (R.Data'Access, "Location: " & Target);
         Put_Line (R.Data'Access, "Content-Length: 0");
         New_Line (R.Data'Access);

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
      return R : Instance do
         Put_Line (R.Data'Access, Constants.Switching_Protocols);
         Put_Line (R.Data'Access, "Upgrade: websocket");
         Put_Line (R.Data'Access, "Connection: Upgrade");
         Put_Line (R.Data'Access, "Sec-WebSocket-Accept: " & Accept_Key);
         New_Line (R.Data'Access);

         R.Complete := True;
      end return;
   end Switch_To_Websocket;
end Black.Response;
