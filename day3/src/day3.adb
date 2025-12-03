pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;

procedure Day3 is
   type Nat_64 is range 0 .. 2 ** 64;
   package Nat_64_IO is new Ada.Text_IO.Integer_IO (Nat_64);

   type Single_Digit is new Natural range 0 .. 9;
   type Digit_Array is array (Positive range <>) of Single_Digit;

   Data_File : File_Type;
   Ch : Character;
   Line : String (1 .. 1000);
   Line_Digits : Digit_Array (1 .. 1000);
   Best_Digits : Digit_Array (1 .. 12);
   Last : Natural;
   Jolt_Sum_A : Nat_64;
   Jolt_Sum_B : Nat_64;
   Highest : Nat_64;

   --  Convert a String to an array of digits in the range 0 .. 9
   procedure Line_To_Digits (Line : String; Line_Digits : out Digit_Array)
   is
      Digit_Value : Natural;
   begin
      Line_Digits := (others => 0);
      for I in 1 .. Line'Length loop
         if Line (I) >= '0' and then Line (I) <= '9'
         then
            Digit_Value := Character'Pos (Line (I)) - Character'Pos ('0');
            if Digit_Value in 0 .. 9 and then
               I <= Line_Digits'Length
            then
               Line_Digits (I) := Single_Digit (Digit_Value);
            end if;
         end if;
      end loop;
   end Line_To_Digits;

   --  Find the highest digit and its position in an array of
   --  digits given a certain starting and ending position
   procedure Find_Highest_In_Range (Line_Digits : Digit_Array;
      Start_Pos : Natural; End_Pos : Natural;
      Highest_Digit : out Single_Digit;
      Highest_Pos : out Natural)
      with
         Pre => (Line_Digits'First = 1 and then
            Start_Pos < Natural'Last and then
            Start_Pos >= 1 and then Start_Pos <= End_Pos
            and then End_Pos <= Line_Digits'Length),
         Post => (Highest_Pos >= Start_Pos and then
            Highest_Pos <= End_Pos)
   is
   begin
      Highest_Pos := Start_Pos;
      Highest_Digit := 0;
      if Start_Pos >= 1 and then Start_Pos <= Line_Digits'Length
      then
         Highest_Digit := Line_Digits (Start_Pos);

         for I in Start_Pos + 1 .. End_Pos loop
            if Line_Digits (I) > Highest_Digit then
               Highest_Digit := Line_Digits (I);
               Highest_Pos := I;
            end if;

            pragma Loop_Invariant (Highest_Pos >= Start_Pos and then
               Highest_Pos <= End_Pos);
         end loop;
      end if;
   end Find_Highest_In_Range;

   procedure Find_Highest_Number (Line_Digits : Digit_Array;
      Best_Digits : in out Digit_Array;
      Highest : out Nat_64)
   is
      Highest_Pos : Natural;
      Start_Pos : Natural;
      Last_Pos : Natural;
      Offset : Natural;
   begin
      Start_Pos := 1;

      --  Find the highest I'th digit
      for I in 1 .. Best_Digits'Length loop

         --  Offset is how many digits from the
         --  end we stop looking
         Offset := (Best_Digits'Length - I);
         if Offset < Line_Digits'Length then
            Last_Pos := Line_Digits'Length - Offset;
            if Start_Pos <= Last_Pos then
               Find_Highest_In_Range (Line_Digits,
                  Start_Pos, Last_Pos,
                  Best_Digits (I), Highest_Pos);
               if Highest_Pos < Natural'Last then
                  Start_Pos := Highest_Pos + 1;
               end if;
            end if;
         end if;
      end loop;

      --  Now that we have the highest digits, turn them into
      --  a number
      Highest := 0;
      for I in 1 .. Best_Digits'Length loop
         if Highest < (Nat_64'Last -
            Nat_64 (Best_Digits (I)) / 10)
         then
            Highest := Highest * 10 + Nat_64 (Best_Digits (I));
         end if;
      end loop;
   end Find_Highest_Number;

begin
   Jolt_Sum_A := 0;
   Jolt_Sum_B := 0;
   Best_Digits := (others => 0);
   Line := (others => '0');

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day3.txt");

   while not End_Of_File (Data_File) loop
      Last := 0;

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Last < Line'Length then
            Last := Last + 1;
            if Last in Line'Range then
               Line (Last) := Ch;
            end if;
         end if;
      end loop;
      Skip_Line (Data_File);

      if Last in Line'Range then
         Line_To_Digits (Line (1 .. Last), Line_Digits);

         Find_Highest_Number (Line_Digits (1 .. Last),
            Best_Digits (1 .. 2), Highest);

         if Jolt_Sum_A < Nat_64'Last - Highest then
            Jolt_Sum_A := Jolt_Sum_A + Highest;
         end if;

         Find_Highest_Number (Line_Digits (1 .. Last),
            Best_Digits (1 .. 12), Highest);

         if Jolt_Sum_B < Nat_64'Last - Highest then
            Jolt_Sum_B := Jolt_Sum_B + Highest;
         end if;
      end if;
   end loop;

   Put ("day3a = ");
   Nat_64_IO.Put (Jolt_Sum_A, Width => 0);
   New_Line;

   Put ("day3b = ");
   Nat_64_IO.Put (Jolt_Sum_B, Width => 0);
   New_Line;

exception
   when others => Put_Line ("Exception");
end Day3;
