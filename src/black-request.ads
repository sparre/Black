with
  Ada.Streams;
with
  Black.HTTP,
  Black.MIME_Types,
  Black.Parameter,
  Black.Parameter.Vectors;

private
with
  Ada.Strings.Unbounded;
private
with
  Black.Optional_HTTP_Method,
  Black.Optional_Natural,
  Black.Optional_String,
  Black.Parsing;

package Black.Request is
   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Method     (Request : in Instance) return HTTP.Methods;
   function Host       (Request : in Instance) return String;
   function Resource   (Request : in Instance) return String;
   function Parameters (Request : in Instance) return Parameter.Vectors.Vector;

   function Has_Parameter (Request : in Instance;
                           Key     : in String) return Boolean;
   function Parameter (Request : in Instance;
                       Key     : in String;
                       Default : in String) return String;
   function Parameter (Request : in Instance;
                       Key     : in String) return String;

   function Has_Origin (Request : in Instance) return Boolean;
   function Origin     (Request : in Instance) return String;

   function Want_Websocket (Request : in Instance) return Boolean;
   function Websocket_Key  (Request : in Instance) return String;

   procedure Generate_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in     Instance);
   function Parse_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Instance;

   function Compose (Method   : in HTTP.Methods;
                     Host     : in String;
                     Resource : in String) return Instance;
   function Compose (Method       : in HTTP.Methods;
                     Host         : in String;
                     Resource     : in String;
                     Content      : in String;
                     Content_Type : in String := MIME_Types.Text.Plain)
                    return Instance;
   function Websocket (Host     : in String;
                       Resource : in String;
                       Key      : in String) return Instance;
private
   procedure Parse_Method_And_Resource
     (Request : in out Instance;
      Line    : in     Ada.Strings.Unbounded.Unbounded_String);
   procedure Parse (Request : in out Instance;
                    Line    : in     Black.Parsing.Header_Line);

   for Instance'Output use Generate_HTTP;
   for Instance'Input  use Parse_HTTP;

   type Instance is tagged
      record
         Method         : Optional_HTTP_Method.Instance;
         Host           : Optional_String.Instance;
         Resource       : Optional_String.Instance;
         Parameters     : Black.Parameter.Vectors.Vector;
         Websocket      : Boolean := False;
         Websocket_Key  : Optional_String.Instance;
         Origin         : Optional_String.Instance;
         Content        : Optional_String.Instance;
         Content_Type   : Optional_String.Instance;
         Content_Length : Optional_Natural.Instance;
      end record;
end Black.Request;
