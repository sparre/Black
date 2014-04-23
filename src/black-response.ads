with
  Ada.Streams;
with
  Black.HTTP,
  Black.MIME_Types;

private
with
  Ada.Strings.Unbounded;

package Black.Response is
   type Instance (<>) is tagged private;
   subtype Class is Instance'Class;

   function Create (Status : HTTP.Statuses) return Instance;

   function Switch_To_Websocket (Key : in String) return Instance;

   function OK (Content_Type : in String;
                Data         : in Ada.Streams.Stream_Element_Array)
               return Instance;
   function OK (Content_Type : in String := MIME_Types.Text.Plain;
                Data         : in String)
               return Instance;

   function No_Content return Instance;

   function Redirect (Target    : in String;
                      Permanent : in Boolean)
                     return Instance;

   function Bad_Request (Content_Type : in String := MIME_Types.Text.Plain;
                         Data         : in String) return Instance;

   function Unauthorized (Content_Type : in String := MIME_Types.Text.Plain;
                          Data         : in String) return Instance;

   function Forbidden (Content_Type : in String := MIME_Types.Text.Plain;
                       Data         : in String) return Instance;

   function Not_Found (Resource : in String) return Instance;
   function Not_Found (Content_Type : in String := MIME_Types.Text.Plain;
                       Data         : in String) return Instance;

   function Server_Error (Content_Type : in String := MIME_Types.Text.Plain;
                          Data         : in String) return Instance;

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

   package Access_Control is
      type HTTP_Method_Set is array (HTTP.Methods) of Boolean;

      procedure Allow_Origin (Item    : in out Class;
                              Pattern : in     String);
      procedure Allow_Credentials (Item : in out Class);
      procedure Allow_Headers (Item    : in out Class;
                               Headers : in     HTTP_Method_Set);
      procedure Max_Age (Item : in out Class;
                         Age  : in     Duration);
   end Access_Control;

   function Status       (Response : in Instance) return HTTP.Statuses;
   function Content_Type (Response : in Instance) return String;
   function Content      (Response : in Instance) return String;
private
   type Optional_Boolean (Set : Boolean := False) is
      record
         case Set is
            when True =>
               Value : Boolean;
            when False =>
               null;
         end case;
      end record;

   type Optional_Duration (Set : Boolean := False) is
      record
         case Set is
            when True =>
               Value : Duration;
            when False =>
               null;
         end case;
      end record;

   type Access_Controls is
      record
         Allow_Origin      : Ada.Strings.Unbounded.Unbounded_String;
         Allow_Credentials : Optional_Boolean;
         Allow_Headers     : Access_Control.HTTP_Method_Set :=
                               (others => False);
         Max_Age           : Optional_Duration;
      end record;

   type Instance (Status : HTTP.Statuses) is tagged
      record
         Content_Type   : Ada.Strings.Unbounded.Unbounded_String;
         Content        : Ada.Strings.Unbounded.Unbounded_String;
         Access_Control : Access_Controls;

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
