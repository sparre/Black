with
  Ada.Characters.Latin_1,
  Ada.Streams,
  Ada.Streams.Stream_IO,
  Ada.Strings.Unbounded;
with
  Black.Stream_Element_Vectors,
  Black.Text_IO;

package body Black.Tests.Text_IO is
   procedure Save (File_Name : in     String;
                   Data      : in     Stream_Element_Vectors.Vector);

   function Load (File_Name : in String) return String;

   procedure Get_Line_Test is
      use Ada.Strings.Unbounded;
      Test_Data : constant array (1 .. 3) of Unbounded_String :=
                    (To_Unbounded_String ("Hello World!"),
                     To_Unbounded_String (""),
                     To_Unbounded_String ("I'm Marvin."));
      Test_File : constant String := "ahven/test_data_4";
   begin
      Save_Test_Data :
      declare
         use Black.Stream_Element_Vectors;
         Buffer : Vector;
      begin
         for I in Test_Data'Range loop
            Black.Text_IO.Put_Line (Target => Buffer,
                                    Item   => To_String (Test_Data (I)));
         end loop;

         Save (File_Name => Test_File,
               Data      => Buffer);
      end Save_Test_Data;

      Load_And_Check_Test_Data :
      declare
         use Ada.Streams.Stream_IO;
         use Ahven;
         Source : File_Type;
         Got    : Unbounded_String;
      begin
         Open (File => Source,
               Name => Test_File,
               Mode => In_File);
         for I in Test_Data'Range loop
            Got := Black.Text_IO.Get_Line (Stream (Source));

            Assert (Got = Test_Data (I),
                    "Line" & Positive'Image (I) & ": Wrote """ &
                    To_String (Test_Data (I)) & """.  Got """ &
                    To_String (Got) & """.");
         end loop;
      end Load_And_Check_Test_Data;
   end Get_Line_Test;

   overriding
   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      T.Set_Name ("Text I/O tests");

      Add_Test_Routine (T, Put_Test'Access,      "Put test");
      Add_Test_Routine (T, Put_Line_Test'Access, "Put_Line test");
      Add_Test_Routine (T, New_Line_Test'Access, "New_Line test");
      Add_Test_Routine (T, Get_Line_Test'Access, "Get_Line test");
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

   New_Line : constant String := Ada.Characters.Latin_1.CR &
                                 Ada.Characters.Latin_1.LF;

   procedure New_Line_Test is
      use Stream_Element_Vectors;
      Test_Data : constant String := "Hello World!";
      Test_File : constant String := "ahven/test_data_3";
      Buffer : Vector;
   begin
      Black.Text_IO.Put_Line (Target => Buffer, Item => Test_Data);
      Black.Text_IO.New_Line (Target => Buffer);
      Black.Text_IO.Put_Line (Target => Buffer, Item => Test_Data);

      Save (File_Name => Test_File,
            Data      => Buffer);

      declare
         use Ahven;
         Expected   : constant String := Test_Data & New_Line &
                                         New_Line &
                                         Test_Data & New_Line;
         Saved_Data : constant String := Load (File_Name => Test_File);
      begin
         Assert (Expected = Saved_Data,
                 "Wrote """ & Expected & """.  Got """ & Saved_Data & """.");
      end;
   end New_Line_Test;

   procedure Put_Line_Test is
      use Stream_Element_Vectors;
      Test_Data : constant String := "Hello World!";
      Test_File : constant String := "ahven/test_data_2";
      Buffer : Vector;
   begin
      Black.Text_IO.Put_Line (Target => Buffer, Item => Test_Data);

      Save (File_Name => Test_File,
            Data      => Buffer);

      declare
         use Ahven;
         Expected   : constant String := Test_Data & New_Line;
         Saved_Data : constant String := Load (File_Name => Test_File);
      begin
         Assert (Expected = Saved_Data,
                 "Wrote """ & Expected & """.  Got """ & Saved_Data & """.");
      end;
   end Put_Line_Test;

   procedure Put_Test is
      use Stream_Element_Vectors;
      Test_Data : constant String := "Hello World!";
      Test_File : constant String := "ahven/test_data_1";
      Buffer : Vector;
   begin
      Black.Text_IO.Put (Target => Buffer, Item => Test_Data);

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
end Black.Tests.Text_IO;
