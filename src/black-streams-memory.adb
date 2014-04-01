package body Black.Streams.Memory is
   not overriding
   function Copy (Stream : in Instance) return Instance is
   begin
      return Result : Instance do
         Result.Data := Stream.Data;
         Result.Next := Stream.Next;
      end return;
   end Copy;

   overriding
   procedure Read (Stream : in out Instance;
                   Item   :    out Ada.Streams.Stream_Element_Array;
                   Last   :    out Ada.Streams.Stream_Element_Offset) is
      use Ada.Streams;
   begin
      Last := Item'First - 1;

      loop
         exit when Stream.Data.Is_Empty;
         exit when Last >= Item'Last;

         declare
            First_Element : Stream_Element_Array
              renames Stream.Data.First_Element;
            Remaining     : Stream_Element_Array
              renames First_Element (Stream.Next .. First_Element'Last);
            Target        : Stream_Element_Array
              renames Item (Last + 1 .. Item'Last);
            Copy          : Stream_Element_Offset;
         begin
            if Remaining'Length <= Target'Length then
               Copy := Remaining'Length;
            else
               Copy := Target'Length;
            end if;

            Target (Target'First .. Target'First + Copy - 1) :=
              Remaining (Remaining'First .. Remaining'First + Copy - 1);

            Last        := Last        + Copy;
            Stream.Next := Stream.Next + Copy;

            if Stream.Next > First_Element'Last then
               Stream.Data.Delete_First;

               if not Stream.Data.Is_Empty then
                  Stream.Next := First_Element'First;
               end if;
            end if;
         end;
      end loop;
   end Read;

   overriding
   procedure Write (Stream : in out Instance;
                    Item   : in     Ada.Streams.Stream_Element_Array) is
   begin
      if Stream.Data.Is_Empty then
         Stream.Next := Item'First;
      end if;

      Stream.Data.Append (Item);
   end Write;
end Black.Streams.Memory;
