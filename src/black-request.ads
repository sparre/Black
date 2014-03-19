with
  Ada.Streams;
with
  Black.HTTP;

private
with
  Ada.Containers.Indefinite_Vectors,
  Ada.Strings.Unbounded;

package Black.Request is
   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Method   (Request : in Instance) return HTTP.Methods;
   function Resource (Request : in Instance) return String;

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

   type Parameter (With_Value : Boolean := True) is
      record
         Key : Ada.Strings.Unbounded.Unbounded_String;
         case With_Value is
            when True =>
               Value : Ada.Strings.Unbounded.Unbounded_String;
            when False =>
               null;
         end case;
      end record;
   package Parameter_Vectors is
      new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                             Element_Type => Parameter);

   type Instance is tagged
      record
         Blank      : Boolean := True;
         Resource   : Ada.Strings.Unbounded.Unbounded_String;
         Method     : HTTP.Methods;
         Parameters : Parameter_Vectors.Vector;
      end record;
end Black.Request;
