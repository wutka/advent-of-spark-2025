pragma Spark_Mode (On);
with Ada.Text_IO; use Ada.Text_IO;

procedure Day9 is
   type Nat_64 is range 0 .. 2 ** 64;
   package Nat_64_IO is new Ada.Text_IO.Integer_IO (Nat_64);
   use Nat_64_IO;

   type Coord_Type is record
      X : Nat_64 := 0;
      Y : Nat_64 := 0;
   end record;

   type Coord_Range is range 1 .. 500;
   type Coord_Array_Type is array (Coord_Range) of Coord_Type;

   Data_File : File_Type;
   X : Nat_64;
   Y : Nat_64;
   Coords : Coord_Array_Type;
   Num_Coords : Coord_Range;
   Ch : Character;
   Area : Nat_64;
   Best_Area : Nat_64;
   Best_Area_B : Nat_64;
   Has_Between : Boolean;

   function Compute_Area (A, B : Coord_Type) return Nat_64
   is
      Width : Nat_64;
      Height : Nat_64;
   begin
      if A.X > B.X then
         Width := A.X - B.X;
      else
         Width := B.X - A.X;
      end if;
      if Width < Nat_64'Last then
         Width := Width + 1;
      end if;

      if A.Y > B.Y then
         Height := A.Y - B.Y;
      else
         Height := B.Y - A.Y;
      end if;
      if Height < Nat_64'Last then
         Height := Height + 1;
      end if;

      if Width > 0 and then Height < Nat_64'Last / Width
      then
         return Width * Height;
      else
         return 0;
      end if;
   end Compute_Area;

   function Is_Between (X, X1, X2 : Nat_64) return Boolean
   is ((X > X1 and then X < X2) or else
       (X > X2 and then X < X1));

   function Is_Inside_Box (A, B, C : Coord_Type) return Boolean
   is (Is_Between (A.X, B.X, C.X) and then Is_Between (A.Y, B.Y, C.Y));

   function Is_Inside_Figure (A : Coord_Type;
      Coords : Coord_Array_Type;
      Num_Coords : Coord_Range) return Boolean
   is
      Intersect_Count : Natural;
      From_Coord : Coord_Type;
      To_Coord : Coord_Type;
   begin
      --  Use odd-even line segment intersects to compute whether
      --  a point is inside the figure or not, going from the left
      --  on the X axis.
      Intersect_Count := 0;
      for I in 1 .. Num_Coords loop
         From_Coord := Coords (I);
         if I = Num_Coords then
            To_Coord := Coords (1);
         else
            To_Coord := Coords (I + 1);
         end if;

         --  Is this a vertical segment and does an imaginary
         --  horizontal line from the left to the point intersect
         --  this segment?
         if (From_Coord.X = To_Coord.X and then
             From_Coord.X <= A.X and then
             ((From_Coord.Y >= A.Y and then To_Coord.Y <= A.Y) or else
              (To_Coord.Y >= A.Y and then From_Coord.Y <= A.Y))) or else
            --  Or is this segment vertical, and if so, does it lie
            --  on the imaginary horizontal line
            (From_Coord.Y = To_Coord.Y and then
             From_Coord.Y = A.Y and then
             A.X >= To_Coord.X and then A.X >= From_Coord.X)
         then
            if Intersect_Count < Natural'Last then
               Intersect_Count := Intersect_Count + 1;
            end if;
         end if;
      end loop;

      --  An odd number of intersects indicates that the
      --  point is contained within the figure
      return (Intersect_Count mod 2 = 1);
   end Is_Inside_Figure;

   function Corners_Inside (A, B : Coord_Type;
      Coords : Coord_Array_Type;
      Num_Coords : Coord_Range) return Boolean
   is
   begin
      --  See if the points immediately adjacent to the corners
      --  lie inside the figure
      if A.X < B.X then
         if not Is_Inside_Figure ((A.X + 1, A.Y), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((A.X + 1, B.Y), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((B.X - 1, B.Y), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((B.X - 1, A.Y), Coords, Num_Coords)
         then
            return False;
         end if;
      elsif A.X > B.X then
         if not Is_Inside_Figure ((B.X + 1, B.Y), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((B.X + 1, A.Y), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((A.X - 1, A.Y), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((A.X - 1, B.Y), Coords, Num_Coords)
         then
            return False;
         end if;
      end if;

      if A.Y < B.Y then
         if not Is_Inside_Figure ((A.X, A.Y + 1), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((B.X, A.Y + 1), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((B.X, B.Y - 1), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((A.X, B.Y - 1), Coords, Num_Coords)
         then
            return False;
         end if;
      elsif A.Y > B.Y then
         if not Is_Inside_Figure ((B.X, B.Y + 1), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((A.X, B.Y + 1), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((A.X, A.Y - 1), Coords, Num_Coords)
            or else
            not Is_Inside_Figure ((B.X, A.Y - 1), Coords, Num_Coords)
         then
            return False;
         end if;
      end if;
      return True;
   end Corners_Inside;

   function No_Passthrough (A, B : Coord_Type;
      Coords : Coord_Array_Type;
      Num_Coords : Coord_Range) return Boolean
   is
      From_Coord : Coord_Type;
      To_Coord : Coord_Type;
   begin
      --  Ensure that no line segments pass through the box. We
      --  already check for end points lying inside the box, but
      --  we need to make sure that endpoints outside the box
      --  don't specify a line that passes through it.
      for I in 1 .. Num_Coords loop
         From_Coord := Coords (I);
         if I = Num_Coords then
            To_Coord := Coords (1);
         else
            To_Coord := Coords (I + 1);
         end if;

         if From_Coord.X = To_Coord.X then
            if Is_Between (From_Coord.X, A.X, B.X) and then
               ((A.Y < B.Y and then
                ((From_Coord.Y < A.Y and then To_Coord.Y > B.Y) or else
                 (To_Coord.Y < A.Y and then From_Coord.Y > B.Y))) or else
                (A.Y > B.Y and then
                 ((From_Coord.Y < B.Y and then To_Coord.Y > A.Y) or else
                 (To_Coord.Y < B.Y and then From_Coord.Y > A.Y))))
            then
               return False;
            end if;
         else
            if Is_Between (From_Coord.Y, A.Y, B.Y) and then
               ((A.X < B.X and then
                ((From_Coord.X < A.X and then To_Coord.X > B.X) or else
                 (To_Coord.X < A.X and then From_Coord.X > B.X))) or else
                (A.X > B.X and then
                 ((From_Coord.X < B.X and then To_Coord.X > A.X) or else
                  (To_Coord.X < B.X and then From_Coord.X > A.X))))
            then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end No_Passthrough;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day9.txt");

   Num_Coords := 1;

   while not End_Of_File (Data_File) loop
      while not End_Of_Line (Data_File) loop
         Get (Data_File, X);
         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);
         exit when End_Of_Line (Data_File);
         if Ch /= ',' then
            Put_Line ("Expected comma");
         end if;

         Get (Data_File, Y);

         Coords (Num_Coords) := (X, Y);

         if Num_Coords < Coord_Range'Last then
            Num_Coords := Num_Coords + 1;
         end if;
      end loop;
      Skip_Line (Data_File);
   end loop;

   if Num_Coords > 1 then
      Num_Coords := Num_Coords - 1;
   end if;

   if Num_Coords = 1 then
      Put_Line ("Not enough coordinates");
      return;
   end if;

   Best_Area := 0;
   Best_Area_B := 0;

   for I in 1 .. Num_Coords - 1 loop
      for J in I + 1 .. Num_Coords loop
         Area := Compute_Area (Coords (I), Coords (J));

         --  Track the best area for part 1
         if Area > Best_Area then
            Best_Area := Area;
         end if;

         --  Track the best area for part 1
         if Area > Best_Area_B then

            --  First make sure that no line segments terminate inside
            --  the box described by coordinates X and Y
            Has_Between := False;
            for K in 1 .. Num_Coords loop
               if K /= I and then K /= J
               then
                  if Is_Inside_Box (Coords (K), Coords (I), Coords (J))
                  then
                     Has_Between := True;
                     exit;
                  end if;
               end if;
            end loop;

            --  Make sure that the opposing corners are also inside
            --  the figure, and then that the points immediately
            --  adjacent all four corners are inside the figure,
            --  and finally, make sure no line segments pass through
            --  the box.
            if not Has_Between then
               if Is_Inside_Figure ((Coords (I).X, Coords (J).Y),
                     Coords, Num_Coords) and then
                  Is_Inside_Figure ((Coords (J).X, Coords (I).Y),
                     Coords, Num_Coords) and then
                  Corners_Inside (Coords (I), Coords (J),
                     Coords, Num_Coords) and then
                  No_Passthrough (Coords (I), Coords (J),
                     Coords, Num_Coords)
               then
                  Best_Area_B := Area;
               end if;
            end if;
         end if;
      end loop;
   end loop;

   Put ("Day 9a = ");
   Put (Best_Area);
   New_Line;

   Put ("Day 9b = ");
   Put (Best_Area_B);
   New_Line;

exception
   when others => Put_Line ("Exception");
end Day9;
