pragma Spark_Mode (On);

package body Joltage is
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

   function Joltages_Valid (Joltages : Joltage_Array_Type;
      Required_Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range) return Boolean
   is
   begin
      for I in 0 .. Max_Joltage_Num loop
         if Joltages (I) > Required_Joltages (I) then
            return False;
         end if;
      end loop;
      return True;
   end Joltages_Valid;

   procedure Try_Presses (Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Tried : in out Tried_Array_Type;
      Lights_Tried : in out Light_Tried_Array_Type;
      Path_Len : Natural;
      Shortest_Path : in out Natural)
   is
      Bit : Lights_Range;
      Best_Light : Light_Number_Range;
      Best_Light_Count : Natural;
      Best_Joltage_Count : Natural;
      Joltage_Diff : Natural;
      Buttons_To_Try : Button_Ref_Array_Type;
      Try_Button_Count : Buttons_Range;
      Buttons_Per_Light : Button_Per_Light_Type;
      Curr_Joltages : Joltage_Array_Type;
      Curr_Path_Len : Natural;
   begin
      Buttons_Per_Light := (others => 0);
      Buttons_To_Try := (others => 1);

      for I in 1 .. Num_Buttons loop
         if not Tried (I) then
            for J in 0 .. Max_Joltage_Num loop
               if not Lights_Tried (J) then
                  Bit := 2 ** Natural (J);
                  if (Buttons (I) and Bit) /= 0 then
                     if Buttons_Per_Light (J) < Natural'Last then
                        Buttons_Per_Light (J) := Buttons_Per_Light (J) + 1;
                     end if;
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      Best_Light := 0;
      Best_Light_Count := Natural'Last;
      Best_Joltage_Count := Natural'Last;

      for I in 0 .. Max_Joltage_Num loop
         if Required_Joltages (I) > Joltages (I) then
            Joltage_Diff := Required_Joltages (I) - Joltages (I);
         else
            Joltage_Diff := 0;
         end if;

         if not Lights_Tried (I) then
            if Buttons_Per_Light (I) > 0 and then
               (Buttons_Per_Light (I) < Best_Light_Count or else
                (Buttons_Per_Light (I) = Best_Light_Count and then
                Joltage_Diff < Best_Joltage_Count))
            then
               Best_Light := I;
               Best_Light_Count := Buttons_Per_Light (I);
               Best_Joltage_Count := Joltage_Diff;
            end if;
         end if;
      end loop;

      if Best_Light_Count > 0 and then
         Best_Light_Count <= Natural (Num_Buttons)
      then
         Try_Button_Count := 1;
         Bit := 2 ** Natural (Best_Light);
         for I in 1 .. Num_Buttons loop
            if not Tried (I) then
               if (Buttons (I) and Bit) /= 0 then
                  Buttons_To_Try (Try_Button_Count) := I;
                  if Try_Button_Count < Num_Buttons then
                     Try_Button_Count := Try_Button_Count + 1;
                  end if;
               end if;
            end if;
         end loop;
         if Try_Button_Count > 1 then
            Try_Button_Count := Try_Button_Count - 1;
         end if;

         Lights_Tried (Best_Light) := True;

         if Try_Button_Count > 1 then
            Try_Buttons (Buttons, Num_Buttons, Buttons_To_Try,
               1, Try_Button_Count, Required_Joltages, Joltages,
               Max_Joltage_Num, Tried, Lights_Tried,
               Best_Light,
               Path_Len, Shortest_Path);
         else
            Tried (Buttons_To_Try (1)) := True;
            if Path_Len < Natural'Last - Best_Joltage_Count then
               Curr_Path_Len := Path_Len + Best_Joltage_Count;
               if Curr_Path_Len < Shortest_Path then
                  Curr_Joltages := Joltages (Joltages'Range);
                  Press (Buttons (Buttons_To_Try (1)),
                     Curr_Joltages, Max_Joltage_Num,
                     Best_Joltage_Count);

                  if Joltages_Valid (Curr_Joltages, Required_Joltages,
                     Max_Joltage_Num)
                  then
                     if Joltages_Match (Curr_Joltages, Required_Joltages,
                        Max_Joltage_Num)
                     then
                        Shortest_Path := Curr_Path_Len;
                     else
                        Try_Presses (Buttons, Num_Buttons,
                           Required_Joltages, Curr_Joltages, Max_Joltage_Num,
                           Tried, Lights_Tried,
                           Curr_Path_Len, Shortest_Path);
                     end if;
                  end if;
               end if;
            end if;
            Tried (Buttons_To_Try (1)) := False;
         end if;

         Lights_Tried (Best_Light) := False;
      end if;
   end Try_Presses;

   procedure Try_Buttons (Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range; Buttons_To_Try : Button_Ref_Array_Type;
      Curr_Try_Button : Buttons_Range; Max_Try_Buttons : Buttons_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Tried : in out Tried_Array_Type;
      Lights_Tried : in out Light_Tried_Array_Type;
      Target_Light : Light_Number_Range;
      Path_Len : Natural;
      Shortest_Path : in out Natural)
   is
      Curr_Joltages : Joltage_Array_Type;
      Presses : Natural;
      Curr_Path_Len : Natural;
      Joltage_Diff : Natural;
   begin
      Presses := Max_Presses (Buttons (Buttons_To_Try (Curr_Try_Button)),
         Required_Joltages, Joltages, Max_Joltage_Num);

      Tried (Buttons_To_Try (Curr_Try_Button)) := True;
      if Curr_Try_Button < Max_Try_Buttons then
         for Press_Amount in reverse 0 .. Presses loop
            if Path_Len < Natural'Last - Press_Amount then
               Curr_Path_Len := Path_Len + Press_Amount;
               if Curr_Path_Len < Shortest_Path then
                  Curr_Joltages := Joltages (Joltages'Range);
                  Press (Buttons (Buttons_To_Try (Curr_Try_Button)),
                     Curr_Joltages, Max_Joltage_Num,
                     Press_Amount);

                  if Joltages_Match (Curr_Joltages, Required_Joltages,
                     Max_Joltage_Num)
                  then
                     Shortest_Path := Curr_Path_Len;
                  elsif Curr_Try_Button < Max_Try_Buttons then
                     Try_Buttons (Buttons, Num_Buttons, Buttons_To_Try,
                        Curr_Try_Button + 1, Max_Try_Buttons,
                        Required_Joltages, Curr_Joltages,
                        Max_Joltage_Num,
                        Tried, Lights_Tried,
                        Target_Light,
                        Curr_Path_Len, Shortest_Path);
                  else
                     Try_Presses (Buttons, Num_Buttons,
                        Required_Joltages, Curr_Joltages, Max_Joltage_Num,
                        Tried, Lights_Tried,
                        Curr_Path_Len, Shortest_Path);
                  end if;
               end if;
            end if;
         end loop;
      else
         if Required_Joltages (Target_Light) > Joltages (Target_Light) then
            Joltage_Diff := Required_Joltages (Target_Light) -
               Joltages (Target_Light);
         else
            Joltage_Diff := 0;
         end if;

         if Path_Len < Natural'Last - Presses then
            Curr_Path_Len := Path_Len + Presses;
            if Curr_Path_Len < Shortest_Path then
               Curr_Joltages := Joltages (Joltages'Range);
               Press (Buttons (Buttons_To_Try (Curr_Try_Button)),
                  Curr_Joltages, Max_Joltage_Num,
                  Joltage_Diff);

               if Joltages_Valid (Curr_Joltages, Required_Joltages,
                  Max_Joltage_Num)
               then
                  if Joltages_Match (Curr_Joltages, Required_Joltages,
                     Max_Joltage_Num)
                  then
                     Shortest_Path := Curr_Path_Len;
                  else
                     Try_Presses (Buttons, Num_Buttons,
                        Required_Joltages, Curr_Joltages, Max_Joltage_Num,
                        Tried, Lights_Tried,
                        Curr_Path_Len, Shortest_Path);
                  end if;
               end if;
            end if;
         end if;
      end if;

      Tried (Buttons_To_Try (Curr_Try_Button)) := False;
   end Try_Buttons;
end Joltage;
