pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Ordered_Sets;

procedure Day2 is
   type Nat_64 is range 0 .. 2 ** 64;
   package Nat64_IO is new Ada.Text_IO.Integer_IO (Nat_64);
   use Nat64_IO;

   package Nat_64_Set is new SPARK.Containers.Formal.Ordered_Sets (
      Element_Type => Nat_64,
      "<" => "<");

   type ID_Str is array (1 .. 15) of Character;

   Bad_ID_Sum : Nat_64;
   Bad_ID_Sum_Extra : Nat_64;

   Data_File : File_Type;
   From_ID_Str : ID_Str;
   From_ID_Length : Natural;
   To_ID_Str : ID_Str;
   To_ID_Length : Natural;

   From_Front : Nat_64;
   To_Front : Nat_64;
   Mult : Nat_64;

   Full_From : Nat_64;
   Full_To : Nat_64;

   Repeated_Twice : Nat_64_Set.Set (100000);

   procedure Read_ID (ID : out ID_Str;
                      ID_Len : out Natural) is
      Ch : Character;
   begin
      ID_Len := 0;
      ID := [others => ' '];

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Ch >= '0' and then Ch <= '9' then
            if ID_Len < ID_Str'Length then
               ID_Len := ID_Len + 1;
               ID (ID_Len) := Ch;
            end if;
         else
            exit;
         end if;
      end loop;
   end Read_ID;

   function Parse_Nat_64 (ID : ID_Str; ID_Len : Natural) return Nat_64
   is
      Result : Nat_64;
      Digit : Nat_64;
   begin
      Result := 0;
      for I in 1 .. ID_Len loop
         if I <= ID'Length and then ID (I) >= '0' and then
            ID (I) <= '9'
         then
            Digit := Nat_64 (Character'Pos (ID (I)) - Character'Pos ('0'));
            if Result < (Nat_64'Last - Digit) / 10 then
               Result := Result * 10 + Digit;
            end if;
         end if;
      end loop;
      return Result;
   end Parse_Nat_64;

   procedure Try_Repeat_Prefix (From_Digits : Nat_64;
      To_Digits : Nat_64;
      Repeats : Natural;
      Mult : Nat_64;
      Full_From : Nat_64;
      Full_To : Nat_64;
      Repeated_Twice : in out Nat_64_Set.Set;
      Bad_Sum : in out Nat_64;
      Bad_Sum_Extra : in out Nat_64)
   is
      ID : Nat_64;
   begin
      if Mult > 0 then
         for Prefix in From_Digits .. To_Digits loop
            ID := Nat_64 (Prefix);
            for Repeat_Count in 2 .. Repeats loop
               if ID < (Nat_64'Last - Nat_64 (Prefix)) / Mult then
                  ID := ID * Mult + Nat_64 (Prefix);
               end if;
            end loop;
            if ID >= Full_From and then ID <= Full_To and then
               not Nat_64_Set.Contains (Repeated_Twice, ID)
            then
               if not Nat_64_Set.Contains (Repeated_Twice, ID)
                  and then Nat_64_Set.Length (Repeated_Twice) <
                     Repeated_Twice.Capacity
               then
                  Nat_64_Set.Insert (Repeated_Twice, ID);
               end if;
               if Repeats = 2 then
                  if Bad_Sum < Nat_64'Last - ID then
                     Bad_Sum := Bad_Sum + ID;
                  end if;
               else
                  if Bad_Sum_Extra < Nat_64'Last - ID then
                     Bad_Sum_Extra := Bad_Sum_Extra + ID;
                  end if;
               end if;
            end if;
         end loop;
      end if;
   end Try_Repeat_Prefix;

   function Get_Mult (Num_Digits : Nat_64) return Nat_64
   is
   begin
      if Num_Digits = 0 then
         return 1;
      elsif Num_Digits = 1 then
         return 10;
      elsif Num_Digits = 2 then
         return 100;
      elsif Num_Digits = 3 then
         return 1000;
      elsif Num_Digits = 4 then
         return 10000;
      elsif Num_Digits = 5 then
         return 100000;
      elsif Num_Digits = 6 then
         return 1000000;
      elsif Num_Digits = 7 then
         return 10000000;
      elsif Num_Digits = 8 then
         return 100000000;
      elsif Num_Digits = 9 then
         return 1000000000;
      else
         return 1;
      end if;
   end Get_Mult;

begin
   Bad_ID_Sum := 0;
   Bad_ID_Sum_Extra := 0;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day2.txt");

   while not End_Of_Line (Data_File) loop
      Read_ID (From_ID_Str, From_ID_Length);
      Read_ID (To_ID_Str, To_ID_Length);

      Full_From := Parse_Nat_64 (From_ID_Str, From_ID_Length);
      Full_To := Parse_Nat_64 (To_ID_Str, To_ID_Length);

      for Repeat_Count in 2 .. 10 loop
         if From_ID_Length mod Repeat_Count = 0 or else
            To_ID_Length mod Repeat_Count = 0
         then
            if From_ID_Length mod Repeat_Count = 0 then
               if From_ID_Length / Repeat_Count < 8 then
                  Mult := Get_Mult (Nat_64 (From_ID_Length / Repeat_Count));
               else
                  Mult := 1;
               end if;

               if From_ID_Length - (From_ID_Length /
                  Repeat_Count) > 0
               then
                  From_Front := Full_From / Get_Mult (Nat_64 (
                     (From_ID_Length - (From_ID_Length / Repeat_Count))));
               else
                  From_Front  := 1;
               end if;
               if From_Front < 1 then
                  From_Front := 1;
               end if;
               if To_ID_Length = From_ID_Length then
                  To_Front := Full_To / Get_Mult (Nat_64 (From_ID_Length -
                     (From_ID_Length / Repeat_Count)));
               else
                  To_Front := Mult - 1;
               end if;

               Try_Repeat_Prefix (From_Front, To_Front, Repeat_Count,
                  Mult, Full_From, Full_To, Repeated_Twice,
                  Bad_ID_Sum, Bad_ID_Sum_Extra);
            elsif To_ID_Length mod Repeat_Count = 0 then
               Mult := Get_Mult (Nat_64 (To_ID_Length / Repeat_Count));
               To_Front := Full_To / Get_Mult (Nat_64 (To_ID_Length -
                  (To_ID_Length / Repeat_Count)));

               if To_ID_Length = From_ID_Length then
                  From_Front := Full_To /
                     Get_Mult (Nat_64 (To_ID_Length -
                        (To_ID_Length / Repeat_Count)));
               else
                  From_Front := Mult / 10;
               end if;

               Try_Repeat_Prefix (From_Front, To_Front, Repeat_Count,
                  Mult, Full_From, Full_To, Repeated_Twice,
                  Bad_ID_Sum, Bad_ID_Sum_Extra);
            end if;
         end if;
      end loop;
   end loop;

   Put ("Day 2a = ");
   Put (Bad_ID_Sum);
   New_Line;
   if Bad_ID_Sum < Nat_64'Last - Bad_ID_Sum_Extra then
      Put ("Day 2b = ");
      Put (Bad_ID_Sum + Bad_ID_Sum_Extra);
      New_Line;
   end if;
exception
   when others => Put_Line ("Exception");
end Day2;
