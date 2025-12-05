pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Ordered_Sets;

procedure Day5 is
   type Nat_64 is range 0 .. 2 ** 64;
   package Nat64_IO is new Ada.Text_IO.Integer_IO (Nat_64);
   use Nat64_IO;

   type ID_Range_Type is record
      Start_ID : Nat_64 := 0;
      End_ID : Nat_64 := 0;
   end record;

   function Range_Less (A, B : ID_Range_Type) return Boolean
   is (A.Start_ID < B.Start_ID or else (A.Start_ID = B.Start_ID
      and then A.End_ID < B.End_ID));

   package Range_Set is new SPARK.Containers.Formal.Ordered_Sets (
      Element_Type => ID_Range_Type,
      "<" => Range_Less);

   Data_File : File_Type;
   Start_ID : Nat_64;
   End_ID : Nat_64;
   Ingredient : Nat_64;
   Ch : Character;
   Line : String (1 .. 200);
   Last : Natural;
   Dash_Pos : Natural;
   Ranges : Range_Set.Set (2000);
   Num_Fresh : Nat_64;
   Num_IDs : Nat_64;
   Last_End : Nat_64;
   Range_Len : Nat_64;

   procedure Parse_Nat64 (Line : String; Val : out Nat_64)
   is
      Digit_Val : Nat_64;
   begin
      Val := 0;
      for I in Line'Range loop
         if Line (I) >= '0' and then Line (I) <= '9'
         then
            Digit_Val := Nat_64 (Character'Pos (Line (I)) -
               Character'Pos ('0'));
            if Val < (Nat_64'Last - Digit_Val) / 10 then
               Val := Val * 10 + Digit_Val;
            end if;
         end if;
      end loop;
   end Parse_Nat64;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day5.txt");

   Line := [others => '0'];

   while not End_Of_File (Data_File) loop
      Last := 0;
      Dash_Pos := 1;
      Start_ID := 0;
      End_ID := 0;

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Last < Line'Length then
            Last := Last + 1;
            if Last in Line'Range then
               Line (Last) := Ch;
               if Ch = '-' then
                  Dash_Pos := Last;
               end if;
            end if;
         end if;
      end loop;
      Skip_Line (Data_File);

      exit when Last = 0;

      if Dash_Pos > 1 and then
         Dash_Pos - 1 in Line'Range
      then
         Parse_Nat64(Line (1 .. Dash_Pos - 1), Start_ID);
      end if;

      if Dash_Pos < Natural'Last and then
         Dash_Pos + 1 in Line'Range and then
         Last in Line'Range
      then
         Parse_Nat64 (Line (Dash_Pos + 1 .. Last), End_ID);
      end if;

      if not Range_Set.Contains (Ranges, (Start_ID, End_ID)) and then
         Range_Set.Length (Ranges) < Ranges.Capacity
      then
         Range_Set.Insert (Ranges, (Start_ID, End_ID));
      end if;
   end loop;

   Num_Fresh := 0;
   Last_End := 0;

   while not End_Of_File (Data_File) loop
      Get (Data_File, Ingredient);
      Skip_Line (Data_File);

      for ID_Range of Ranges loop
         exit when ID_Range.Start_ID > Ingredient;

         if Ingredient >= ID_Range.Start_ID and then
            Ingredient <= ID_Range.End_ID
         then
            if Num_Fresh < Nat_64'Last then
               Num_Fresh := Num_Fresh + 1;
               exit;
            end if;
         end if;
      end loop;
   end loop;

   Num_IDs := 0;
   for ID_Range of Ranges loop
      if ID_Range.Start_ID <= ID_Range.End_ID and then
         ID_Range.Start_ID > Last_End and then
         ID_Range.End_ID < Nat_64'Last
      then
         Range_Len := ID_Range.End_ID - ID_Range.Start_ID + 1;
         if Num_IDs < Nat_64'Last - Range_Len then
            Num_IDs := Num_IDs + Range_Len;
         end if;
         Last_End := ID_Range.End_ID + 1;
      elsif Last_End >= ID_Range.Start_ID and then
         Last_End <= ID_Range.End_ID and then
         ID_Range.End_ID < Nat_64'Last
      then
         Range_Len := ID_Range.End_ID - Last_End + 1;
         if Num_IDs < Nat_64'Last - Range_Len then
            Num_IDs := Num_IDs + Range_Len;
         end if;
         Last_End := ID_Range.End_ID + 1;
      end if;
   end loop;

   Put ("Day 5a = ");
   Put (Num_Fresh);
   New_Line;

   Put ("Day 5b = ");
   Put (Num_IDs);
   New_Line;
exception
   when others => Put_Line ("Exception");
end Day5;
