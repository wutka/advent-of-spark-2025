pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Visit; use Visit;
with Joltage; use Joltage;

procedure Day10 is
   Data_File : File_Type;
   Initial_Light : Lights_Range;
   Light_Mask : Lights_Range;
   Num_Lights : Natural;
   Buttons : Button_Array_Type;
   Ch : Character;
   Curr_Button : Lights_Range;
   Bit_Number : Natural;
   N : Integer;
   Num_Buttons : Buttons_Range;
   Bit_Value : Lights_Range;
   Parse_Valid : Boolean;
   Shortest_Path : Natural;
   Sum_A : Natural;
   Sum_B : Natural;
   Required_Joltages : Joltage_Array_Type;
   Joltages : Joltage_Array_Type;
   Joltage_Number : Light_Number_Range;
   Max_Joltage_Num : Light_Number_Range;
   Tried : Tried_Array_Type;
   Lights_Tried : Light_Tried_Array_Type;

   procedure Expect (Data_File : File_Type;
      Target : Character; Valid : out Boolean)
   is
      Ch : Character;
   begin
      Valid := True;

      if not End_Of_Line (Data_File) then
         Get (Data_File, Ch);

         if Ch /= Target then
            Put ("Expected ");
            if Target = ' ' then
               Put ("space");
            else
               Put (Target);
            end if;
            Put (" got ");
            if Ch = ' ' then
               Put  ("space");
            else
               Put (Ch);
            end if;
            New_Line;
            Valid := False;
         end if;
      else
         Valid := False;
      end if;
   end Expect;

begin
   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day10.txt");

   Sum_A := 0;
   Sum_B := 0;
   Buttons := (others => 0);
   Required_Joltages := (others => 0);
   Max_Joltage_Num := 0;

   while not End_Of_File (Data_File) loop
      Initial_Light := 0;
      Num_Buttons := 1;

      if not End_Of_Line (Data_File) then
         Expect (Data_File, '[', Parse_Valid);
         exit when not Parse_Valid;
      end if;

      Light_Mask := 0;
      Num_Lights := 0;
      while not End_Of_Line (Data_File) loop
         Get (Data_File, Ch);
         if Ch = '#' then
            if Initial_Light <= (Lights_Range'Last - 1) / 2 then
               Initial_Light := Initial_Light * 2 + 1;
            end if;
            if Light_Mask < (Lights_Range'Last - 1) / 2 then
               Light_Mask := Light_Mask * 2 + 1;
            end if;
            if Num_Lights < Natural'Last then
               Num_Lights := Num_Lights + 1;
            end if;
         elsif Ch = '.' then
            if Initial_Light <= Lights_Range'Last / 2 then
               Initial_Light := Initial_Light * 2;
            end if;
            if Light_Mask < (Lights_Range'Last - 1) / 2 then
               Light_Mask := Light_Mask * 2 + 1;
            end if;
            if Num_Lights < Natural'Last then
               Num_Lights := Num_Lights + 1;
            end if;
         else
            exit;
         end if;
      end loop;

      while not End_Of_Line (Data_File) loop
         Expect (Data_File, ' ', Parse_Valid);
         exit when not Parse_Valid;

         exit when End_Of_Line (Data_File);

         Get (Data_File, Ch);
         if Ch = '(' then
            Curr_Button := 0;

            while not End_Of_Line (Data_File) loop
               Get (Data_File, N);
               if N in Natural'Range then
                  Bit_Number := N;
               else
                  Put_Line ("Invalid bit number");
                  Bit_Number := 0;
               end if;

               if Num_Lights <= 11 and then Bit_Number < Num_Lights then
                  Bit_Value := 2 ** (Num_Lights - Bit_Number - 1);
                  if Curr_Button < Lights_Range'Last - Bit_Value then
                     Curr_Button := Curr_Button + Bit_Value;
                  end if;
               end if;

               if End_Of_Line (Data_File) then
                  Put_Line ("Unexpected end of line.");
                  return;
               end if;

               Get (Data_File, Ch);
               if Ch = ')' then
                  Buttons (Num_Buttons) := Curr_Button;
                  if Num_Buttons < Buttons_Range'Last then
                     Num_Buttons := Num_Buttons + 1;
                  end if;
                  exit;
               end if;
            end loop;
         elsif Ch = '{' then
            if Num_Lights > 0 and then
               Num_Lights - 1 <= Natural (Light_Number_Range'Last)
            then
               Max_Joltage_Num := Light_Number_Range (Num_Lights - 1);
               Joltage_Number := Light_Number_Range (Num_Lights - 1);
            else
               exit;
            end if;

            while not End_Of_Line (Data_File) loop
               Get (Data_File, N);
               if N in Natural'Range then
                  Required_Joltages (Joltage_Number) := Natural (N);
                  if Joltage_Number > 0 then
                     Joltage_Number := Joltage_Number - 1;
                  end if;
               end if;

               exit when End_Of_Line (Data_File);

               Get (Data_File, Ch);
               exit when Ch = '}';
            end loop;

            Skip_Line (Data_File);
            exit;
         end if;
      end loop;

      if Num_Buttons > 1 then
         Num_Buttons := Num_Buttons - 1;
      else
         Put_Line ("No buttons");
         return;
      end if;

      Visit.Visit (Initial_Light, Light_Mask, Buttons, Num_Buttons,
         Shortest_Path);

      if Sum_A < Natural'Last - Shortest_Path then
         Sum_A := Sum_A + Shortest_Path;
      end if;

      Tried := (others => False);
      Lights_Tried := (others => False);
      Joltages := (others => 0);
      Shortest_Path := Natural'Last;
      Try_Presses (Buttons, Num_Buttons,
         Required_Joltages, Joltages, Max_Joltage_Num, Tried,
         Lights_Tried, 0, Shortest_Path);

      if Sum_B < Natural'Last - Shortest_Path then
         Sum_B := Sum_B + Shortest_Path;
      end if;
   end loop;

   Put ("Day 10a = ");
   Put (Sum_A);
   New_Line;

   Put ("Day 10b = ");
   Put (Sum_B);
   New_Line;
exception
   when others => Put_Line ("Exception");
end Day10;
