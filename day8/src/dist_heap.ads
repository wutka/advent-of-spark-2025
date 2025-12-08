pragma Spark_Mode (On);

with Simple_Binary_Heap;

package Dist_Heap is

   type Nat_64 is range 0 .. 2 ** 64;

   type Coord_Range is range 1 .. 1000;

   type Dist_Type is record
      From_Pos : Coord_Range;
      To_Pos : Coord_Range;
      Dist : Nat_64;
   end record;
   
   function Dist_Less (A, B : Dist_Type) return Boolean
   is (A.Dist < B.Dist or else (A.Dist = B.Dist and then
      A.From_Pos < B.From_Pos) or else
      (A.Dist = B.Dist and A.From_Pos = B.From_Pos and
       A.To_Pos < B.To_Pos));

   package Dist_Heap is new Simple_Binary_Heap (
      Element_Type => Dist_Type,
      Default_Element => (1, 1, Nat_64'Last),
      "<" => Dist_Less);

   Dists : Dist_Heap.Binary_Heap (1000000);
end Dist_Heap;
