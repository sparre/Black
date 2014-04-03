with
  Ada.Streams;

private
with
  Ada.Strings.Unbounded;
private
with
  Black.HTTP;

package Black.Response is
   type Instance (<>) is tagged private;
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

   function Input_HTTP
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
     return Instance;

   procedure Output
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : in              Class);

   for Instance'Output       use Output_HTTP;
   for Instance'Input        use Input_HTTP;
   for Instance'Class'Output use Output;
private
   type Instance (Status : HTTP.Statuses) is tagged
      record
         Content_Type : Ada.Strings.Unbounded.Unbounded_String;
         Content      : Ada.Strings.Unbounded.Unbounded_String;

         case Status is
            when HTTP.Switching_Protocols =>
               Websocket_Accept : HTTP.Websocket_Accept_Key;
            when HTTP.Moved_Permanently | HTTP.Moved_Temporarily =>
               Location : Ada.Strings.Unbounded.Unbounded_String;
            when others =>
               null;
         end case;
      end record;
end Black.Response;
