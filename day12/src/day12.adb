pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Day12 is
   Data_File : File_Type;
   Width : Natural;
   Height : Natural;
   Digit_Val : Natural;
   Size_Sum : Natural;
   Package_Count : Integer;
   Ch : Character;
   Cap_Count : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day12.txt");

   Cap_Count := 0;

   while not End_Of_File (Data_File) loop
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Ch >= '0' and then Ch <= '9'
         then
            Width := Character'Pos (Ch) - Character'Pos ('0');
         else
            exit;
         end if;
         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);
         if Ch >= '0' and then Ch <= '9'
         then
            Digit_Val := Character'Pos (Ch) - Character'Pos ('0');
            if Width < (Natural'Last - Digit_Val) / 10 then
               Width := Width * 10 + Digit_Val;
            end if;
         else
            exit;
         end if;
         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);
         if Ch /= 'x' then
            exit;
         end if;

         Get (Data_File, Ch);
         if Ch >= '0' and then Ch <= '9'
         then
            Height := Character'Pos (Ch) - Character'Pos ('0');
         else
            exit;
         end if;
         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);
         if Ch >= '0' and then Ch <= '9'
         then
            Digit_Val := Character'Pos (Ch) - Character'Pos ('0');
            if Height < (Natural'Last - Digit_Val) / 10 then
               Height := Height * 10 + Digit_Val;
            end if;
         else
            exit;
         end if;
         exit when End_Of_Line (Data_File);
         Get (Data_File, Ch);
         if Ch /= ':' then
            exit;
         end if;

         Size_Sum := 0;
         while not End_Of_Line (Data_File) loop
            Get (Data_File, Package_Count);
            if Package_Count > 0 and then Package_Count < 100 and then
               Size_Sum < Natural'Last - Package_Count * 7
            then
               Size_Sum := Size_Sum + Package_Count * 7;
            end if;
         end loop;
         if Width < 100 and then Height < 100 and then
            Size_Sum < Width * Height and then
            Cap_Count < Natural'Last
         then
            Cap_Count := Cap_Count + 1;
         end if;
      end loop;
      Skip_Line (Data_File);
   end loop;

   Put ("Day12a = ");
   Put (Cap_Count);
   New_Line;
exception
   when others => Put_Line ("Exception");
end Day12;
