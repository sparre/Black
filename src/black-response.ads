with
  Ada.Streams;

private
with
  Black.Streams.Memory;

package Black.Response is
   type Instance is tagged limited private;
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

   procedure Output_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Instance);

   procedure Output
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Class);

   for Instance'Output       use Output_HTTP;
   for Instance'Class'Output use Output;
private
   type Instance is tagged limited
      record
         Data     : aliased Streams.Memory.Instance;
         Complete : Boolean := False;
      end record;
end Black.Response;
