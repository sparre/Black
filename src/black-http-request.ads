with
  Ada.Exceptions;
private
with
  POSIX.IO;

package Black.HTTP.Request is
   type Instance is limited private;

   function URL      (Request : in Instance) return String;
   function Resource (Request : in Instance) return String;
   function Method   (Request : in Instance) return Methods;

   procedure Redirect (Request   : in out Instance;
                       Target    : in     String;
                       Permanent : in     Boolean);

   procedure Server_Error
     (Request : in out Instance;
      Error   : in     Ada.Exceptions.Exception_Occurrence);

   procedure Commit (Request : in out Instance);
   function Committed (Request : in Instance) return Boolean;
private
   type Instance is limited
      record
         Connection : POSIX.IO.File_Descriptor;
         Committed  : Boolean;
      end record;
end Black.HTTP.Request;
