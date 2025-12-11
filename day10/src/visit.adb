pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Visit is

   procedure Visit (Initial_Light : Lights_Range;
      Light_Mask : Lights_Range;
      Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range;
      Shortest_Path : out Natural)
   is
      Curr_Visit : Visit_Pos_Type;
      New_Visit : Visit_Pos_Type;
      Initial_Visit : Visit_Pos_Type;
      Next_Light : Lights_Range;
   begin
      Visit_Heap.Clear (Visits);

      Shortest_Path := Natural'Last;

      Initial_Visit := (Initial_Light, (others => False), 0);
      Initial_Visit.Visited (Initial_Light) := True;

      if Visit_Heap.Has_Capacity (Visits) then
         Visit_Heap.Insert (Visits, Initial_Visit);
      end if;

      while not Visit_Heap.Is_Empty (Visits) loop
         Visit_Heap.Top (Visits, Curr_Visit);

         for B in 1 .. Num_Buttons loop
            Next_Light := (Curr_Visit.Light_Value xor Buttons (B))
               and Light_Mask;
            if Next_Light = 0 and then
               Curr_Visit.Path_Length < Shortest_Path and then
               Curr_Visit.Path_Length + 1 < Shortest_Path
            then
               Shortest_Path := Curr_Visit.Path_Length + 1;
            elsif not Curr_Visit.Visited (Next_Light) and then
               Curr_Visit.Path_Length < Shortest_Path
            then
               New_Visit := (Next_Light,
                  Curr_Visit.Visited (Curr_Visit.Visited'First ..
                                       Curr_Visit.Visited'Last),
                  Curr_Visit.Path_Length);
               New_Visit.Visited (Next_Light) := True;
               if New_Visit.Path_Length < Natural'Last then
                  New_Visit.Path_Length := New_Visit.Path_Length + 1;
               end if;
               if Visit_Heap.Has_Capacity (Visits) then
                  Visit_Heap.Insert (Visits, New_Visit);
               end if;
            end if;
         end loop;
      end loop;
   end Visit;
end Visit;
