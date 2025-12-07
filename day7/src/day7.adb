pragma Spark_Mode (On);
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Day7 is
   type Nat_64 is range 0 .. 2**64;
   package LL_IO is new Ada.Text_IO.Integer_IO (Nat_64);

   Data_File : File_Type;
   Ch : Character;
   Split_Count : Natural;
   Split_Count_B : Nat_64;
   Has_Beam : array (1 .. 200) of Boolean;
   Parent_Count : array (1 .. 200) of Nat_64;
   Col : Natural;
   Max_Col : Natural;
begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day7.txt");

   Has_Beam := (others => False);
   Parent_Count := (others => 0);
   Split_Count := 0;

   Max_Col := 1;

   while not End_Of_File (Data_File) loop
      Col := 1;
      Max_Col := 1;

      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);

         if Ch = 'S' then
            Has_Beam (Col) := True;
            Parent_Count (Col) := 1;
         elsif Ch = '^' and then Has_Beam (Col) then
            if Split_Count < Natural'Last then
               Split_Count := Split_Count + 1;
            end if;
            if Col > 1 then
               Has_Beam (Col - 1) := True;
               if Parent_Count (Col - 1) < Nat_64'Last -
                  Parent_Count (Col)
               then
                  Parent_Count (Col - 1) := Parent_Count (Col - 1) +
                     Parent_Count (Col);
               end if;
            end if;
            if Col < Has_Beam'Last then
               Has_Beam (Col + 1) := True;
               if Parent_Count (Col + 1) < Nat_64'Last -
                  Parent_Count (Col)
               then
                  Parent_Count (Col + 1) := Parent_Count (Col + 1) +
                     Parent_Count (Col);
               end if;
            end if;
            Has_Beam (Col) := False;
            Parent_Count (Col) := 0;
         end if;

         if Col < Has_Beam'Last then
            Col := Col + 1;
            Max_Col := Col;
         end if;

         pragma Loop_Invariant (Col in Has_Beam'Range and then
            Max_Col in Has_Beam'Range);
      end loop;
      Skip_Line (Data_File);
   end loop;

   Split_Count_B := 0;
   for Col in 1 .. Max_Col loop
      if Split_Count_B < Nat_64'Last -
         Parent_Count (Col) then
         Split_Count_B := Split_Count_B + Parent_Count (Col);
      end if;
   end loop;

   Put ("Day 7a = ");
   Put (Split_Count);
   New_Line;

   Put ("Day 7b = ");
   LL_IO.Put (Split_Count_B);
   New_Line;
exception
   when others => Put_Line ("Exception");

end Day7;
