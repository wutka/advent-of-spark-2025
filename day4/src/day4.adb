pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Ordered_Sets;

procedure Day4 is
   type Coord_Type is record
      X : Integer := 0;
      Y : Integer := 0;
   end record;

   function Coord_Less (A, B : Coord_Type) return Boolean
   is (A.X < B.X or else (A.X = B.X and then A.Y < B.Y));

   package Coord_Set is new SPARK.Containers.Formal.Ordered_Sets (
      Element_Type => Coord_Type,
      "<" => Coord_Less);

   Data_File : File_Type;
   Coords : Coord_Set.Set (100000);
   X : Natural;
   Y : Natural;
   Max_X : Natural;
   Max_Y : Natural;
   Ch : Character;
   Roll_Count_A : Natural;
   Roll_Count_B : Natural;
   Adj_Count : Natural;
   Removed : Boolean;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day4.txt");

   Y := 0;
   Max_X := 0;
   Max_Y := 0;
   while not End_Of_File (Data_File) loop
      X := 0;
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Ch = '@' then
            if not Coord_Set.Contains (Coords, (X => X, Y => Y)) and then
               Coord_Set.Length (Coords) < Coords.Capacity
            then
               Coord_Set.Insert (Coords, (X => X, Y => Y));
            end if;
         end if;
         if X < Natural'Last then
            X := X + 1;
         end if;
      end loop;
      Skip_Line (Data_File);
      if X > 0 then
         X := X - 1;
         if X > Max_X then
            Max_X := X;
         end if;
      end if;
      if Y < Natural'Last then
         Y := Y + 1;
      end if;
   end loop;
   if Y > 0 then
      Y := Y - 1;
      Max_Y := Y;
   end if;

   Put (Max_X);
   Put (" ");
   Put (Max_Y);
   New_Line;

   Roll_Count_A := 0;
   if Max_Y < Integer'Last and then Max_X < Integer'Last then
      for CY in 0 .. Max_Y loop
         for CX in 0 .. Max_X loop
            if Coord_Set.Contains (Coords, (X => CX, Y => CY)) then
               Adj_Count := 0;
               if Coord_Set.Contains (Coords, (X => CX - 1, Y => CY - 1))
               then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX, Y => CY - 1)) then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX + 1, Y => CY - 1))
               then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX - 1, Y => CY)) then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX + 1, Y => CY)) then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX - 1, Y => CY + 1))
               then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX, Y => CY + 1)) then
                  Adj_Count := Adj_Count + 1;
               end if;
               if Coord_Set.Contains (Coords, (X => CX + 1, Y => CY + 1))
               then
                  Adj_Count := Adj_Count + 1;
               end if;

               if Adj_Count < 4 and then Roll_Count_A < Natural'Last
               then
                  Roll_Count_A := Roll_Count_A + 1;
               end if;
            end if;
         end loop;
      end loop;
   end if;

   Roll_Count_B := 0;
   if Max_Y < Integer'Last and then Max_X < Integer'Last then
      Removed := True;
      while Removed loop
         Removed := False;
         for CY in 0 .. Max_Y loop
            for CX in 0 .. Max_X loop
               if Coord_Set.Contains (Coords, (X => CX, Y => CY)) then
                  Adj_Count := 0;
                  if Coord_Set.Contains (Coords, (X => CX - 1, Y => CY - 1))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX, Y => CY - 1))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX + 1, Y => CY - 1))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX - 1, Y => CY))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX + 1, Y => CY))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX - 1, Y => CY + 1))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX, Y => CY + 1))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;
                  if Coord_Set.Contains (Coords, (X => CX + 1, Y => CY + 1))
                  then
                     Adj_Count := Adj_Count + 1;
                  end if;

                  if Adj_Count < 4 and then Roll_Count_B < Natural'Last
                  then
                     Roll_Count_B := Roll_Count_B + 1;
                     Removed := True;
                     Coord_Set.Exclude (Coords, (X => CX, Y => CY));
                  end if;
               end if;
            end loop;
         end loop;
      end loop;
   end if;
   Put ("day4a = ");
   Put (Roll_Count_A, Width => 0);
   New_Line;
   Put ("day4b = ");
   Put (Roll_Count_B, Width => 0);
   New_Line;

exception
   when others => Put_Line ("Exception");
end Day4;
