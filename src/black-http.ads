package Black.HTTP is
   pragma Pure;

   Version : constant String;

   type Methods is (Options, Get, Head, Post, Put, Delete, Trace, Connect);

   type Statuses is (Switching_Protocols,
                     OK,
                     No_Content,
                     Moved_Permanently,
                     Moved_Temporarily,
                     Bad_Request,
                     Unauthorized,
                     Forbidden,
                     Not_Found,
                     Server_Error);

   function Status_Line (Status : in Statuses) return String;

   type Header_Key is new String;
   overriding
   function "=" (Left, Right : in Header_Key) return Boolean;

   type Websocket_Accept_Key is new String (1 .. 28);
private
   Version : constant String := "HTTP/1.1";
end Black.HTTP;
