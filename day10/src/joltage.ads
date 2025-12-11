pragma Spark_Mode (On);

with Visit; use Visit;

package Joltage is
   type Tried_Array_Type is array (Buttons_Range) of Boolean;
   type Light_Tried_Array_Type is array (Light_Number_Range) of Boolean;
   type Button_Per_Light_Type is array (Light_Number_Range) of Natural;
   type Button_Ref_Array_Type is array (Buttons_Range) of Buttons_Range;

   function Max_Presses (Button : Lights_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range) return Natural;

   procedure Press (Button : Lights_Range;
      Joltages : in out Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Presses : Natural);

   function Joltages_Match (Joltages : Joltage_Array_Type;
      Required_Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range) return Boolean;

   procedure Try_Presses (Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Tried : in out Tried_Array_Type;
      Lights_Tried : in out Light_Tried_Array_Type;
      Path_Len : Natural;
      Shortest_Path : in out Natural);

   procedure Try_Buttons (Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range; Buttons_To_Try : Button_Ref_Array_Type;
      Curr_Try_Button : Buttons_Range; Max_Try_Buttons : Buttons_Range;
      Required_Joltages : Joltage_Array_Type;
      Joltages : Joltage_Array_Type;
      Max_Joltage_Num : Light_Number_Range;
      Tried : in out Tried_Array_Type;
      Lights_Tried : in out Light_Tried_Array_Type;
      Path_Len : Natural;
      Shortest_Path : in out Natural);
end Joltage;
