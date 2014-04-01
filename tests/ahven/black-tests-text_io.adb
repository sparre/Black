with
  Ada.Characters.Latin_1,
  Ada.Strings.Unbounded;
with
  Black.Streams.Memory,
  Black.Text_IO;

package body Black.Tests.Text_IO is
   procedure Get_Line_Test is
      use Ada.Strings.Unbounded;
      Test_Data : constant array (1 .. 3) of Unbounded_String :=
                    (To_Unbounded_String ("Hello World!"),
                     To_Unbounded_String (""),
                     To_Unbounded_String ("I'm Marvin."));
      Buffer    : aliased Streams.Memory.Instance;
   begin
      Save_Test_Data :
      declare
         use Black.Text_IO;
      begin
         for I in Test_Data'Range loop
            Put_Line (Target => Buffer'Access,
                      Item   => To_String (Test_Data (I)));
         end loop;
      end Save_Test_Data;

      Load_And_Check_Test_Data :
      declare
         use Ahven;
         use Black.Text_IO;
         Got    : Unbounded_String;
      begin
         for I in Test_Data'Range loop
            Got := Get_Line (Buffer'Access);

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

   New_Line : constant String := Ada.Characters.Latin_1.CR &
                                 Ada.Characters.Latin_1.LF;

   procedure New_Line_Test is
      Test_Data : constant String := "Hello World!";
      Buffer    : aliased Streams.Memory.Instance;
   begin
      Black.Text_IO.Put_Line (Target => Buffer'Access, Item => Test_Data);
      Black.Text_IO.New_Line (Target => Buffer'Access);
      Black.Text_IO.Put_Line (Target => Buffer'Access, Item => Test_Data);

      declare
         use Ahven;
         Expected   : constant String := Test_Data & New_Line &
                                         New_Line &
                                         Test_Data & New_Line;
         Saved_Data : String (1 .. Expected'Length);
      begin
         String'Read (Buffer'Access, Saved_Data);

         Assert (Expected = Saved_Data,
                 "Wrote """ & Expected & """.  Got """ & Saved_Data & """.");
      end;
   end New_Line_Test;

   procedure Put_Line_Test is
      Test_Data : constant String := "Hello World!";
      Buffer    : aliased Streams.Memory.Instance;
   begin
      Black.Text_IO.Put_Line (Target => Buffer'Access, Item => Test_Data);

      declare
         use Ahven;
         Expected   : constant String := Test_Data & New_Line;
         Saved_Data : String (1 .. Expected'Length);
      begin
         String'Read (Buffer'Access, Saved_Data);

         Assert (Expected = Saved_Data,
                 "Wrote """ & Expected & """.  Got """ & Saved_Data & """.");
      end;
   end Put_Line_Test;

   procedure Put_Test is
      Test_Data : constant String := "Hello World!";
      Buffer    : aliased Streams.Memory.Instance;
   begin
      Black.Text_IO.Put (Target => Buffer'Access, Item => Test_Data);

      declare
         use Ahven;
         Saved_Data : String (1 .. Test_Data'Length);
      begin
         String'Read (Buffer'Access, Saved_Data);

         Assert (Test_Data = Saved_Data,
                 "Wrote """ & Test_Data & """.  Got """ & Saved_Data & """.");
      end;
   end Put_Test;
end Black.Tests.Text_IO;
