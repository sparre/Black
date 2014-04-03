with
  Ada.Characters.Handling;

package body Black.HTTP is
   overriding
   function "=" (Left, Right : in Header_Key) return Boolean is
      use Ada.Characters.Handling;
   begin
      return To_Lower (String (Left)) = To_Lower (String (Right));
   end "=";

   function Status_Line (Status : in Statuses) return String is
   begin
      pragma Style_Checks ("M100"); --  Nice tabular format:
      case Status is
         when Switching_Protocols => return Version & " 101 Switching Protocols";
         when OK                  => return Version & " 200 OK";
         when No_Content          => return Version & " 204 No Content";
         when Moved_Permanently   => return Version & " 301 Moved Permanently";
         when Moved_Temporarily   => return Version & " 302 Found";
         when Bad_Request         => return Version & " 400 Bad Request";
         when Unauthorized        => return Version & " 401 Not Authorized";
         when Forbidden           => return Version & " 403 Forbidden";
         when Not_Found           => return Version & " 404 Not Found";
         when Server_Error        => return Version & " 500 Internal Server Error";
      end case;
   end Status_Line;
end Black.HTTP;
