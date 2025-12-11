pragma Spark_Mode (On);

package body Simple_Binary_Heap is

   procedure Top (Heap : in out Binary_Heap;
      Top_Elem : out Element_Type)
   is
   begin
      Top_Elem := Heap.Nodes (1);
      Heap.Length := Heap.Length - 1;
      if Heap.Length > 0 then
         Heap.Nodes (1) := Heap.Nodes (Heap.Length + 1);
         if Heap.Length > 1 then
            Rebalance_Down (Heap, 1);
         end if;
      end if;
   end Top;

   procedure Clear (Heap : out Binary_Heap)
   is
   begin
      Heap.Length := 0;
      Heap.Nodes := (others => Default_Element);
   end Clear;

   function Parent_Loc (Pos : Natural) return Natural
   is
   begin
      if Pos <= 1 then
         return 1;
      else
         return Pos / 2;
      end if;
   end Parent_Loc;

   function Left_Child (Pos : Natural) return Natural
   is
   begin
      return 2 * Pos;
   end Left_Child;

   function Right_Child (Pos : Natural) return Natural
   is
   begin
      return 2 * Pos + 1;
   end Right_Child;

   procedure Insert (Heap : in out Binary_Heap;
      New_Elem : Element_Type)
   is
   begin
      Heap.Length := Heap.Length + 1;
      Heap.Nodes (Heap.Length) := New_Elem;

      if Heap.Length > 1 then
         Rebalance_Up (Heap, Heap.Length);
      end if;
   end Insert;

   procedure Rebalance_Down (Heap : in out Binary_Heap;
      Changed_Pos : Natural)
   is
      Left, Right : Natural;
      Temp : Element_Type;
   begin
      if Changed_Pos > Heap.Length / 2 then
         return;
      end if;

      if Changed_Pos > 0 and then
         Changed_Pos <= Heap.Length / 2
      then
         Left := Left_Child (Changed_Pos);
         Right := Right_Child (Changed_Pos);

         if Left > 0 and then
            Left < Heap.Length and then
            Right > 0 and then
            Right <= Heap.Length
         then
            if Heap.Nodes (Left) < Heap.Nodes (Changed_Pos) then
               if Heap.Nodes (Right) < Heap.Nodes (Left) then
                  Temp := Heap.Nodes (Right);
                  Heap.Nodes (Right) := Heap.Nodes (Changed_Pos);
                  Heap.Nodes (Changed_Pos) := Temp;
                  Rebalance_Down (Heap, Right);
               else
                  Temp := Heap.Nodes (Left);
                  Heap.Nodes (Left) := Heap.Nodes (Changed_Pos);
                  Heap.Nodes (Changed_Pos) := Temp;
                  Rebalance_Down (Heap, Left);
               end if;
            elsif Heap.Nodes (Right) < Heap.Nodes (Changed_Pos) then
               Temp := Heap.Nodes (Right);
               Heap.Nodes (Right) := Heap.Nodes (Changed_Pos);
               Heap.Nodes (Changed_Pos) := Temp;
               Rebalance_Down (Heap, Right);
            end if;
         elsif Left > 0 and then
            Left <= Heap.Length
         then
            if Heap.Nodes (Left) < Heap.Nodes (Changed_Pos) then
               Temp := Heap.Nodes (Left);
               Heap.Nodes (Left) := Heap.Nodes (Changed_Pos);
               Heap.Nodes (Changed_Pos) := Temp;
               Rebalance_Down (Heap, Left);
            end if;
         end if;
      end if;
   end Rebalance_Down;

   procedure Rebalance_Up (Heap : in out Binary_Heap;
      Changed_Pos : Natural)
   is
      Parent : Natural;
      Temp : Element_Type;
   begin
      Parent := Parent_Loc (Changed_Pos);
      if Parent in 1 .. Heap.Length and then
         Parent /= Changed_Pos and then
         Heap.Nodes (Changed_Pos) < Heap.Nodes (Parent)
      then
            Temp := Heap.Nodes (Parent);
            Heap.Nodes (Parent) := Heap.Nodes (Changed_Pos);
            Heap.Nodes (Changed_Pos) := Temp;
            Rebalance_Up (Heap, Parent);
      end if;
   end Rebalance_Up;
end Simple_Binary_Heap;
