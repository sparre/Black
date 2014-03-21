with
  Ada.Streams;

private
with
  Black.Stream_Element_Vectors;

package Black.Response is
   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Redirect (Target    : in String;
                      Permanent : in Boolean)
                     return Class;

   function OK (Content_Type : in String;
                Data         : in Ada.Streams.Stream_Element_Array)
               return Class;
   function OK (Data : in String)
               return Class;

   function Not_Found (Resource : in String) return Class;

   function Switch_To_Websocket (Key : in String) return Class;
private
   procedure Output_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Instance);

   for Instance'Output use Output_HTTP;

   type Instance is tagged
      record
         Data     : Stream_Element_Vectors.Vector;
         Complete : Boolean := False;
      end record;
end Black.Response;
