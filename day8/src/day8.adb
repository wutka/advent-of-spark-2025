pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Dist_Heap; use Dist_Heap;

procedure Day8 is
   package LL_IO is new Ada.Text_IO.Integer_IO (Nat_64);
   use LL_IO;

   type Coord_Type is record
      X : Nat_64 := 0;
      Y : Nat_64 := 0;
      Z : Nat_64 := 0;
      Group : Coord_Range := 1;
   end record;

   function Safe_Dist_Square (X1, X2 : Nat_64) return Nat_64
   is
      Dist : Nat_64;
   begin
      if X1 >= X2 then
         Dist := X1 - X2;
      else
         Dist := X2 - X1;
      end if;

      if Dist > 0 and then Dist < Nat_64'Last / Dist
      then
         return Dist * Dist;
      else
         return Dist;
      end if;
   end Safe_Dist_Square;

   function Coord_Sq_Dist (A, B : Coord_Type) return Nat_64
   is
      Dist : Nat_64;
      Sq : Nat_64;
   begin
      Dist := 0;

      Sq := Safe_Dist_Square (A.X, B.X);
      if Dist < Nat_64'Last - Sq then
         Dist := Dist + Sq;
      end if;

      Sq := Safe_Dist_Square (A.Y, B.Y);
      if Dist < Nat_64'Last - Sq then
         Dist := Dist + Sq;
      end if;

      Sq := Safe_Dist_Square (A.Z, B.Z);
      if Dist < Nat_64'Last - Sq then
         Dist := Dist + Sq;
      end if;

      return Dist;
   end Coord_Sq_Dist;

   Data_File : File_Type;
   Coords : array (Coord_Range) of Coord_Type;
   Group_Sizes : array (Coord_Range) of Natural;
   Top_Sizes : array (1 .. 3) of Natural;
   Num_Coords : Coord_Range;
   X, Y, Z : Nat_64;
   Ch : Character;
   Dist : Nat_64;
   Dist_Rec : Dist_Type;
   Best_From_Pos : Coord_Range;
   Best_To_Pos : Coord_Range;
   Best_From_Group : Coord_Range;
   Best_To_Group : Coord_Range;
   Got_First : Boolean;
   Sum : Natural;
   Num_Groups : Natural;
   Result_B : Nat_64;
   Num_Circuits : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day8.txt");

   Num_Coords := 1;
   Coords := (others => (0, 0, 0, 1));
   Group_Sizes := (others => 1);
   Top_Sizes := (others => 0);

   Got_First := False;

   while not End_Of_File (Data_File) loop
      while not End_Of_Line (Data_File) loop
         Get (Data_File, X);

         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);
         if Ch /= ',' then
            Put_Line ("Expected comma");
         end if;

         exit when End_Of_Line (Data_File);

         Get (Data_File, Y);

         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);

         if Ch /= ',' then
            Put_Line ("Expected comma");
         end if;

         exit when End_Of_Line (Data_File);

         Get (Data_File, Z);

         if Got_First and then Num_Coords < Coord_Range'Last
         then
            Num_Coords := Num_Coords + 1;
         else
            Got_First := True;
         end if;

         Coords (Num_Coords) := (X, Y, Z, Num_Coords);
      end loop;

      Skip_Line (Data_File);
   end loop;

   if Num_Coords > 1 then
      for I in 1 .. Num_Coords - 1 loop
         for J in I + 1 .. Num_Coords loop
            Dist := Coord_Sq_Dist (Coords (I), Coords (J));
            Dist_Rec := (I, J, Dist);
            if Dist_Heap.Dist_Heap.Has_Capacity (Dists) then
               Dist_Heap.Dist_Heap.Insert (Dists, Dist_Rec);
            else
               Put_Line ("Heap out of capacity");
            end if;
         end loop;
      end loop;

      Num_Circuits := 0;
      Num_Groups := 1000;

      while not Dist_Heap.Dist_Heap.Is_Empty (Dists) loop
         Dist_Heap.Dist_Heap.Top (Dists, Dist_Rec);

         Best_From_Pos := Dist_Rec.From_Pos;
         Best_To_Pos := Dist_Rec.To_Pos;
         Best_From_Group := Coords (Best_From_Pos).Group;
         Best_To_Group := Coords (Best_To_Pos).Group;

         if Best_From_Group /= Best_To_Group then
            for J in Coord_Range loop
               if Coords (J).Group = Best_To_Group then
                  Coords (J).Group := Best_From_Group;
               end if;
            end loop;
            if Group_Sizes (Best_From_Group) < Natural'Last -
               Group_Sizes (Best_To_Group)
            then
               Group_Sizes (Best_From_Group) :=
                  Group_Sizes (Best_From_Group) +
                  Group_Sizes (Best_To_Group);
            end if;
            Group_Sizes (Best_To_Group) := 0;

            if Num_Groups > 0 then
               Num_Groups := Num_Groups - 1;
            end if;

            if Num_Groups = 1 then
               Put ("day8b = ");
               if Coords (Best_From_Pos).X > 0 and then
                  Coords (Best_To_Pos).X < Nat_64'Last /
                     Coords (Best_From_Pos).X
               then
                  Result_B := Coords (Best_From_Pos).X *
                     Coords (Best_To_Pos).X;
                  Put (Result_B, Width => 0);
               else
                  Put (0);
               end if;
               New_Line;
               exit;
            end if;
         end if;

         if Num_Circuits < Natural'Last then
            Num_Circuits := Num_Circuits + 1;
         end if;

         if Num_Circuits = 1000 then
            for I in Coord_Range loop
               if Group_Sizes (I) > Top_Sizes (1) then
                  Top_Sizes (3) := Top_Sizes (2);
                  Top_Sizes (2) := Top_Sizes (1);
                  Top_Sizes (1) := Group_Sizes (I);
               elsif Group_Sizes (I) > Top_Sizes (2) then
                  Top_Sizes (3) := Top_Sizes (2);
                  Top_Sizes (2) := Group_Sizes (I);
               elsif Group_Sizes (I) > Top_Sizes (3) then
                  Top_Sizes (3) := Group_Sizes (I);
               end if;
            end loop;
            Sum := Top_Sizes (1);
            if Top_Sizes (2) > 0 and then
               Sum < Natural'Last / Top_Sizes (2)
            then
               Sum := Sum * Top_Sizes (2);
            end if;
            if Top_Sizes (3) > 0 and then
               Sum < Natural'Last / Top_Sizes (3)
            then
               Sum := Sum * Top_Sizes (3);
            end if;
            Put ("Day 8a = ");
            Put (Nat_64 (Sum), Width => 0);
            New_Line;
         end if;
      end loop;
   end if;
exception
   when others => Put_Line ("Exception");
end Day8;
