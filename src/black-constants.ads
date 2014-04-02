private
package Black.Constants is
   pragma Pure;

   pragma Style_Checks ("M100"); --  Nice tabular format:

   Version             : constant String := "HTTP/1.1";

   Switching_Protocols : constant String := Version & " 101 Switching Protocols";

   OK                  : constant String := Version & " 200 OK";
   No_Content          : constant String := Version & " 204 No Content";

   Moved_Permanently   : constant String := Version & " 301 Moved Permanently";
   Moved_Temporarily   : constant String := Version & " 302 Found";

   Bad_Request         : constant String := Version & " 400 Bad Request";
   Unauthorized        : constant String := Version & " 401 Not Authorized";
   Forbidden           : constant String := Version & " 403 Forbidden";
   Not_Found           : constant String := Version & " 404 Not Found";

   Server_Error        : constant String := Version & " 500 Internal Server Error";
end Black.Constants;
