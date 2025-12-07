pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
procedure Day6 is
   type Nat_64 is range 0 .. 2 ** 64;
   package Nat64_IO is new Ada.Text_IO.Integer_IO (Nat_64);
   use Nat64_IO;

   type Col_Width_Type is range 1 .. 5;
   type Num_Grid_Type is array (1 .. 1000, Col_Width_Type) of Nat_64;
   type Column_Width_Array_Type is array (0 .. 1000) of Col_Width_Type;
   type Op_List_Type is array (1 .. 1000) of Character;

   Data_File : File_Type;
   Num_Grid : Num_Grid_Type;
   Num_Grid2 : Num_Grid_Type;
   Ops : Op_List_Type;
   Ch : Character;
   Row : Col_Width_Type;
   Col : Natural;
   Col_B : Col_Width_Type;
   Curr_Width : Col_Width_Type;
   Col_Widths : Column_Width_Array_Type;
   Digit_Val : Nat_64;
   Max_Col : Natural;
   Max_Row : Col_Width_Type;
   Num : Nat_64;
   Num_B : Nat_64;
   Grid_Val : Nat_64;
   Sum : Nat_64;
   Sum_B : Nat_64;
   Found_Op_Row : Boolean;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day6.txt");

   Max_Row := 1;
   Max_Col := 1;
   Found_Op_Row := False;
   Col_Widths := (others => 1);
   Ops := (others => ' ');

   while not End_Of_File (Data_File) loop
      Col := 0;
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if not Found_Op_Row and then Ch /= '+' and then Ch /= '*' and then
            Max_Row < 4
         then
            Max_Row := Max_Row + 1;
            Skip_Line (Data_File);
         elsif Ch = '+' or else Ch = '*'
         then
            Found_Op_Row := True;
            if Col < 1000 then
               if Col > 0 and then Col_Widths (Col) > 1
               then
                  Col_Widths (Col) := Col_Widths (Col) - 1;
               end if;
               Col := Col + 1;
               Max_Col := Col;
            end if;
            Col_Widths (Col) := 1;
            Ops (Col) := Ch;
         elsif Ch = ' ' then
            if Col > 0 and then Col_Widths (Col) < 5 then
               Col_Widths (Col) := Col_Widths (Col) + 1;
            end if;
         end if;
         pragma Loop_Invariant (Col in 0 .. 1000);
      end loop;
      Skip_Line (Data_File);
   end loop;
   Close (Data_File);
   if Is_Open (Data_File) then
      Put_Line ("File did not close");
   end if;

   for Row in Col_Width_Type'Range loop
      for Col in 1 .. 1000 loop
         Num_Grid (Col, Row) := 0;
         Num_Grid2 (Col, Row) := 0;
      end loop;
   end loop;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day6.txt");

   Row := 1;
   Sum := 0;
   Sum_B := 0;

   while Row <= Max_Row and then
      not End_Of_File (Data_File)
   loop
      Col := 1;
      Curr_Width := 1;
      Num := 0;

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Col in 1 .. 1000 and then Curr_Width <= Col_Widths (Col)
         then
            if Ch >= '0' and then Ch <= '9'
            then
               Digit_Val := Nat_64 (Character'Pos (Ch) -
                  Character'Pos ('0'));
               if Num < (Nat_64'Last - Digit_Val) / 10 then
                  Num := Num * 10 + Digit_Val;
                  Num_Grid (Col, Row) := Num;
               end if;

               Col_B := Col_Widths (Col) - Curr_Width + 1;
               if Num_Grid2 (Col, Col_B) < (Nat_64'Last - Digit_Val) / 10
               then
                  Num_Grid2 (Col, Col_B) := Num_Grid2 (Col, Col_B) * 10 +
                     Digit_Val;
               end if;
            end if;
            if Curr_Width < 5 then
               Curr_Width := Curr_Width + 1;
            end if;
         else
            if Max_Col <= 1000 and then Col < Max_Col then
               Col := Col + 1;
               Curr_Width := 1;
               Num := 0;
            end if;
         end if;
         pragma Loop_Invariant (Col in 0 .. 1000 and then
            Curr_Width in 1 .. Col_Widths (Col) + 1);
      end loop;
      if Row < 4 then
         Row := Row + 1;
      end if;
      Skip_Line (Data_File);
   end loop;

   if Max_Col in 1 .. 1000 then
      for Col in 1 .. Max_Col loop
         if Ops (Col) = '+' then
            Num := 0;
            for Row in 1 .. Max_Row loop
               if Num < Nat_64'Last - Num_Grid (Col, Row) then
                  Num := Num + Num_Grid (Col, Row);
               end if;
            end loop;
            if Sum < Nat_64'Last - Num then
               Sum := Sum + Num;
            end if;
         elsif Ops (Col) = '*' then
            Num := 1;
            for Row in 1 .. Max_Row loop
               if Num_Grid (Col, Row) > 0 and then
                  Num < Nat_64'Last / Num_Grid (Col, Row)
               then
                  Num := Num * Num_Grid (Col, Row);
               end if;
            end loop;
            if Sum < Nat_64'Last - Num then
               Sum := Sum + Num;
            end if;
         end if;
      end loop;

      for Col in 1 .. Max_Col loop
         if Ops (Col) = '+' then
            Num_B := 0;
            for N in 1 .. Col_Widths (Col) loop
               if Num_B < Nat_64'Last - Num_Grid2 (Col, N) then
                  Num_B := Num_B + Num_Grid2 (Col, N);
               end if;
            end loop;
            if Sum_B < Nat_64'Last - Num_B then
               Sum_B := Sum_B + Num_B;
            end if;
         elsif Ops (Col) = '*' then
            Num_B := 1;
            for N in 1 .. Col_Widths (Col) loop
               Grid_Val := 1;
               if Num_Grid2 (Col, N) < 100000 then
                  Grid_Val := Num_Grid2 (Col, N);
                  if Grid_Val > 0 then
                     if Num_B > 0 and then
                        Num_B < 100000000000000
                     then
                        Num_B := Num_B * Grid_Val;
                     end if;
                  end if;
               end if;
               pragma Loop_Invariant (Grid_Val < 100000);
            end loop;
            if Sum_B < Nat_64'Last - Num_B then
               Sum_B := Sum_B + Num_B;
            end if;
         end if;
      end loop;
   end if;

   Put ("day6a = ");
   Put (Sum);
   New_Line;

   Put ("day6b = ");
   Put (Sum_B);
   New_Line;
exception
   when others => Put_Line ("Exception");
end Day6;
