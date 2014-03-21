with
  Ada.Streams;
with
  Black.HTTP,
  Black.Parameter,
  Black.Parameter.Vectors;

private
with
  Ada.Strings.Unbounded;

package Black.Request is
   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Method     (Request : in Instance) return HTTP.Methods;
   function Resource   (Request : in Instance) return String;
   function Parameters (Request : in Instance) return Parameter.Vectors.Vector;

   Protocol_Error : exception;

   function Parse_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Instance;
private
   procedure Parse_Method_And_Resource
     (Request : in out Instance;
      Line    : in     Ada.Strings.Unbounded.Unbounded_String);
   procedure Parse
     (Request : in out Instance;
      Line    : in     Ada.Strings.Unbounded.Unbounded_String);

   for Instance'Input use Parse_HTTP;

   type Instance is tagged
      record
         Blank      : Boolean := True;
         Resource   : Ada.Strings.Unbounded.Unbounded_String;
         Method     : HTTP.Methods;
         Parameters : Parameter.Vectors.Vector;
      end record;
end Black.Request;
