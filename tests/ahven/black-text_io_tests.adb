with
  Ada.Streams,
  Ada.Streams.Stream_IO,
  Ada.Strings.Unbounded;
with
  Black.Stream_Element_Vectors,
  Black.Text_IO;

package body Black.Text_IO_Tests is
   procedure Save (File_Name : in     String;
                   Data      : in     Stream_Element_Vectors.Vector);

   function Load (File_Name : in String) return String;

   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("Text I/O tests");

      Add_Test_Routine (T, Put_Test'Access,      "Put test");
      Add_Test_Routine (T, Put_Line_Test'Access, "Put_Line test");
      Add_Test_Routine (T, New_Line_Test'Access, "New_Line test");
   end Initialize;

   function Load (File_Name : in     String) return String is
      use Ada.Streams.Stream_IO, Ada.Strings.Unbounded;
      Source : File_Type;
      Buffer : Unbounded_String;
   begin
      Open (File => Source,
            Name => File_Name,
            Mode => In_File);
      loop
         Append (Buffer, Character'Input (Stream (Source)));
      end loop;
   exception
      when End_Error =>
         Close (File => Source);
         return To_String (Buffer);
   end Load;

   procedure New_Line_Test is
   begin
      raise Program_Error with "New_Line test not implemented.";
   end New_Line_Test;

   procedure Put_Line_Test is
   begin
      raise Program_Error with "Put_Line test not implemented.";
   end Put_Line_Test;

   procedure Put_Test is
      use Stream_Element_Vectors, Text_IO;
      Test_Data : constant String := "Hello World!";
      Test_File : constant String := "ahven/test_data_1";
      Buffer : Vector;
   begin
      Put (Target => Buffer, Item => Test_Data);

      Save (File_Name => Test_File,
            Data      => Buffer);

      declare
         use Ahven;
         Saved_Data : constant String := Load (File_Name => Test_File);
      begin
         Assert (Test_Data = Saved_Data,
                 "Wrote """ & Test_Data & """.  Got """ & Saved_Data & """.");
      end;
   end Put_Test;

   procedure Save (File_Name : in     String;
                   Data      : in     Stream_Element_Vectors.Vector) is
      use Ada.Streams.Stream_IO;
      use Stream_Element_Vectors;
      Target : File_Type;
   begin
      Create (File => Target,
              Name => File_Name);
      for Index in Data.First_Index .. Data.Last_Index loop
         Stream (Target).Write (Data.Element (Index));
      end loop;
      Close (File => Target);
   end Save;
end Black.Text_IO_Tests;
