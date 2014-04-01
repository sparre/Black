package Black.HTTP is
   pragma Pure;

   type Methods is (PUT, GET, DELETE, POST, OPTIONS);

private

   HTTP_Part    : constant String := "HTTP/1.1";

   OK           : constant String := HTTP_Part & " 200 OK";
   No_Content   : constant String := HTTP_Part & " 204 No Content";

   Bad_Request  : constant String := HTTP_Part & " 400 Bad Request";
   Unauthorized : constant String := HTTP_Part & " 401 Not Authorized";
   Forbidden    : constant String := HTTP_Part & " 403 Forbidden";
   Not_Found    : constant String := HTTP_Part & " 404 Not Found";

   Server_Error : constant String := HTTP_Part & " 500 Internal Server Error";

end Black.HTTP;
