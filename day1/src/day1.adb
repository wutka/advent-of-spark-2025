pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Integer_Text_IO;

procedure Day1 is
   type Dial_Value is mod 100;

   Data_File : File_Type;
   Dial : Dial_Value;
   Dist_Mod : Dial_Value;
   Zero_Count_A : Natural;
   Zero_Count_B : Natural;
   Full_Turns : Natural;
   Dir : Character;
   Dist_Int : Integer;
   Dist : Natural;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day1.txt");

   Dial := 50;
   Zero_Count_A := 0;
   Zero_Count_B := 0;

   while not End_Of_File (Data_File) loop
      --  Read the direction
      Get (Data_File, Dir);

      --  Read the number of clicks
      Get (Data_File, Dist_Int);

      if Dist_Int > 0 then
         Dist := Natural (Dist_Int);
      else
         Dist := 0;
      end if;

      --  For part B, see how many full turns are made
      Full_Turns := Dist / 100;

      Dist_Mod := Dial_Value (Dist mod 100);

      --  Add the full turns to the count for B since
      --  we know it will have to pass through 0
      if Zero_Count_B < Natural'Last - Full_Turns then
         Zero_Count_B := Zero_Count_B + Full_Turns;
      end if;

      if Dir = 'R' then
         --  If adding the clicks causes the Dial to be less than it
         --  was, we know it passes 0
         if Dist_Mod > 0 and then Dial + Dist_Mod < Dial and then
            Zero_Count_B < Natural'Last
         then
            Zero_Count_B := Zero_Count_B + 1;
         end if;

         --  Move the dial
         Dial := Dial + Dist_Mod;
      else
         --  If subtracting the clicks causes the dial to be
         --  higher than it was, we know it passes 0
         --  But, if we started at 0, we don't want to
         --  count it twice.
         --  If turning left gets us exactly to 0, count that
         if Dist_Mod > 0 and then
            ((Dial > 0 and then Dial - Dist_Mod > Dial)
              or else Dial = Dist_Mod) and then
            Zero_Count_B < Natural'Last
         then
            Zero_Count_B := Zero_Count_B + 1;
         end if;

         --  Move the dial
         Dial := Dial - Dist_Mod;
      end if;

      --  Count the number of times it is exactly 0
      if Dial = 0 and then Zero_Count_A < Natural'Last
      then
         Zero_Count_A := Zero_Count_A + 1;
      end if;
      Skip_Line (Data_File);
   end loop;

   Close (Data_File);
   if Is_Open (Data_File) then
      Put_Line ("Error closing file");
   end if;

   Put ("Day1a = ");
   Put (Zero_Count_A);
   New_Line;

   Put ("Day1b = ");
   Put (Zero_Count_B);
   New_Line;
exception
   when others =>
      Put_Line ("I/O Error.");
end Day1;
