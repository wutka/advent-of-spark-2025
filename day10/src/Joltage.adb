   function Max_Presses (Button : Lights_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range) return Natural
   is
      Presses : Natural;
      Bit : Lights_Range;
   begin
      Presses := Natural'Last;

      for I in 0 .. Max_Joltage_Num loop
         Bit := 2 ** Natural (I);
         if (Button and Bit) /= 0 then
            if Joltages (I) <= Required_Joltages (I) then
               if Required_Joltages (I) - Joltages (I) < Presses then
                  Presses := Required_Joltages (I) - Joltages (I);
               end if;
            else
               Presses := 0;
            end if;
         end if;
      end loop;

      return Presses;
   end Max_Presses;

   procedure Press (Button : Lights_Range;
      Joltages : in out Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Presses : Natural)
   is
      Bit : Lights_Range;
   begin
      for I in 0 .. Max_Joltage_Num loop
         Bit := 2 ** Natural (I);
         if (Button and Bit) /= 0 then
            if Joltages (I) < Natural'Last - Presses then
               Joltages (I) := Joltages (I) + Presses;
            end if;
         end if;
      end loop;
   end Press;

   function Joltages_Match (Joltages : Joltage_Array_Type;
      Required_Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range) return Boolean
   is
   begin
      for I in 0 .. Max_Joltage_Num loop
         if Joltages (I) /= Required_Joltages (I) then
            return False;
         end if;
      end loop;
      return True;
   end Joltages_Match;

   procedure Try_Presses (Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Tried : in out Tried_Array_Type;
      Path_Len : Natural;
      Shortest_Path : in out Natural)
   is
      Bit : Lights_Range;
      Best_Light : Light_Number_Range;
      Best_Light_Count : Natural;
      Buttons_To_Try : Button_Ref_Array_Type;
      Try_Button_Count : Buttons_Range;
      Curr_Joltages : Joltage_Array_Type;
      Presses : Natural;
      Buttons_Per_Light : Button_Per_Light_Type;
   begin
      Buttons_Per_Light := (others => 0);

      for I in 1 .. Num_Buttons loop
         if not Tried (I) then
            for J in 0 .. Max_Joltage_Num loop
               Bit := 2 ** Natural (J);
               if (Buttons (I) and Bit) /= 0 then
                  if Buttons_Per_Light (J) < Natural'Last then
                     Buttons_Per_Light (J) := Buttons_Per_Light (J) + 1;
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      Best_Light := 0;
      Best_Light_Count := Natural'Last;

      for I in 0 .. Max_Joltage_Num loop
         if Buttons_Per_Light (I) < Best_Light_Count then
            Best_Light := I;
            Best_Light_Count := Buttons_Per_Light (I);
         end if;
      end loop;

      if Best_Light_Count > 0 and then
         Best_Light_Count <= Natural (Num_Buttons)
      then
         Try_Button_Count := 1;
         Bit := 2 ** Natural (Best_Light);
         for I in 1 .. Buttons_Range (Best_Light_Count) loop
            if (Buttons (I) and Bit) /= 0 then
               Buttons_To_Try (Try_Button_Count) := I;
               if Try_Button_Count < Num_Buttons then
                  Try_Button_Count := Try_Button_Count + 1;
               end if;
            end if;
         end loop;

         Try_Buttons (Buttons, Num_Buttons, Buttons_To_Try,
            1, Try_Button_Count, Required_Joltages, Joltages,
            Max_Joltage_Num, Tried, Path_Len, Shortest_Path);
      end if;
   end Try_Presses;

   procedure Try_Buttons (Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range; Try_Buttons : Button_Ref_Array_Type;
      Curr_Try_Button : Buttons_Range; Max_Try_Buttons : Buttons_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Tried : in out Tried_Array_Type;
      Path_Len : Natural;
      Shortest_Path : in out Natural)
   is
      Curr_Joltages : Joltage_Array_Type;
      Presses : Natural;
      Curr_Path_Len : Natural;
   begin
      Presses := Max_Presses (Buttons (Try_Buttons (Curr_Try_Button)),
         Required_Joltages, Joltages, Max_Joltage_Num);

      Tried (Try_Buttons (Curr_Try_Button)) := True;
      for Press_Amount in reverse 0 .. Presses loop
         if Curr_Path_Len < Natural'Last - Presses then
            Curr_Path_Len := Path_Len + Press_Amount;
            if Curr_Path_Len < Shortest_Path then
               Curr_Joltages := Joltages (Joltages'Range);
               Press (Buttons (Try_Buttons (Curr_Try_Button)),
                  Curr_Joltages, Required_Joltages, Max_Joltage_Num,
                  Press_Amount);

               if Joltages_Match (Curr_Joltages, Required_Joltages) then
                  Shortest_Path := Curr_Path_Len;
               elsif Curr_Try_Button < Max_Try_Buttons then
                  Try_Buttons (Buttons, Num_Buttons, Try_Buttons,
                     Curr_Try_Button + 1, Max_Try_Buttons,
                     Required_Joltages, Curr_Joltages,
                     Tried, Curr_Path_Len, Shortest_Path);
               else
                  Try_Presses (Buttons, Num_Buttons,
                     Required_Joltages, Curr_Joltages, Max_Joltage_Num,
                     Tried, Curr_Path_Len, Shortest_Path);
               end if;
            end if;
         end if;
      end loop;
      Tried (Try_Buttons (Curr_Try_Button)) := False;
   end Try_Buttons;
