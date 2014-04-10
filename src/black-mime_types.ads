package Black.MIME_Types is
   package Application is
      JSON : constant String := "application/json";
      Raw  : constant String := "application/x-raw";
   end Application;

   package Text is
      Plain : constant String := "text/plain; charset=iso-8859-1";
   end Text;
end Black.MIME_Types;
