pragma Spark_Mode (On);

generic
   type Element_Type is private;
   Default_Element : Element_Type;
   with function "<" (X, Y : Element_Type) return Boolean;

package Simple_Binary_Heap is

   pragma Unevaluated_Use_Of_Old (Allow);

   type Binary_Heap (Capacity : Natural) is private;

   function Has_Capacity (Heap : Binary_Heap) return Boolean with
      Post => Has_Capacity'Result = (Length (Heap) < Capacity (Heap));

   function Is_Empty (Heap : Binary_Heap) return Boolean with
      Post => Is_Empty'Result = (Length (Heap) = 0);

   function Length (Heap : Binary_Heap) return Natural with
      Post => Length'Result >= 0 and then Length'Result <= Capacity (Heap);

   function Capacity (Heap : Binary_Heap) return Natural;

   procedure Top (Heap : in out Binary_Heap;
      Top_Elem : out Element_Type)
      with
         Pre => not Is_Empty (Heap),
         Post => Length (Heap) = Length (Heap'Old) - 1;

   procedure Insert (Heap : in out Binary_Heap;
      New_Elem : Element_Type)
      with
         Pre => Has_Capacity (Heap),
         Post => Length (Heap) = Length (Heap'Old) + 1;

private
   type Nodes_Array is array (Natural range <>) of Element_Type;

   type Binary_Heap (Capacity : Natural) is record
      Nodes : Nodes_Array (1 .. Capacity) := (others => Default_Element);
      Length : Natural := 0;
   end record with
      Type_Invariant => Is_Valid (Binary_Heap);

   function Is_Valid (Heap : Binary_Heap) return Boolean
   is (Heap.Length <= Heap.Capacity);

   function Has_Capacity (Heap : Binary_Heap) return Boolean
   is (Heap.Length < Heap.Capacity);

   function Is_Empty (Heap : Binary_Heap) return Boolean
   is (Heap.Length = 0);

   function Length (Heap : Binary_Heap) return Natural
   is (Heap.Length);

   function Capacity (Heap : Binary_Heap) return Natural
   is (Heap.Capacity);

   function Parent_Loc (Pos : Natural) return Natural;

   function Left_Child (Pos : Natural) return Natural
      with
         Pre => Pos <= Natural'Last / 2;

   function Right_Child (Pos : Natural) return Natural
      with
         Pre => Pos <= Natural'Last / 2;

   procedure Rebalance_Down (Heap : in out Binary_Heap;
      Changed_Pos : Natural)
      with
         Pre => Heap.Length <= Heap.Capacity and then
            Changed_Pos in 1 .. Heap.Capacity,
         Post => Heap.Length = Heap'Old.Length;

   procedure Rebalance_Up (Heap : in out Binary_Heap;
      Changed_Pos : Natural)
      with
         Pre => Heap.Length <= Heap.Capacity and then
            Changed_Pos in 1 .. Heap.Capacity,
         Post => Heap.Length = Heap'Old.Length;

end Simple_Binary_Heap;
