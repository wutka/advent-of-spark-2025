pragma Spark_Mode (On);

with Simple_Binary_Heap;

package Visit is
   type Lights_Range is mod 2 ** 11;
   type Buttons_Range is range 1 .. 15;
   type Light_Number_Range is range 0 .. 9;
   type Button_Press is range 0 .. 300;

   type Visited_Array_Type is array (Lights_Range) of Boolean;
   type Button_Array_Type is array (Buttons_Range) of Lights_Range;
   type Button_Num_Array_Type is array (Buttons_Range) of Buttons_Range;
   type Joltage_Array_Type is array (Light_Number_Range) of Natural;

   type Visit_Pos_Type is record
      Light_Value : Lights_Range;
      Visited : Visited_Array_Type;
      Path_Length : Natural;
   end record;

   function Visit_Less (A, B : Visit_Pos_Type) return Boolean
   is (A.Path_Length < B.Path_Length);

   package Visit_Heap is new Simple_Binary_Heap (
      Element_Type => Visit_Pos_Type,
      Default_Element => (0, (others => False), 0),
      "<" => Visit_Less);

   procedure Visit (Initial_Light : Lights_Range;
      Light_Mask : Lights_Range;
      Buttons : Button_Array_Type;
      Num_Buttons : Buttons_Range;
      Shortest_Path : out Natural);

   Visits : Visit_Heap.Binary_Heap (2048);
end Visit;
