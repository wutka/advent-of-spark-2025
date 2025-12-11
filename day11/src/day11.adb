pragma Spark_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with SPARK.Containers.Formal.Ordered_Maps;

procedure Day11 is
   type Nat_64 is range 0 .. 2 ** 64;
   package Nat_64_IO is new Ada.Text_IO.Integer_IO (Nat_64);
   use Nat_64_IO;

   type ID_Type is new String (1 .. 3);
   type ID_Num_Range is range 0 .. 700;

   type Connection_List_Range is range 0 .. 30;
   type Connection_List_Type is array (Connection_List_Range)
      of ID_Num_Range;

   type Connection_Type is record
      Num_Connections : Connection_List_Range;
      Connections : Connection_List_Type;
   end record;

   type Connection_Array_Type is array (ID_Num_Range) of Connection_Type;

   package ID_Map is new SPARK.Containers.Formal.Ordered_Maps (
      Key_Type => ID_Type, Element_Type => ID_Num_Range,
      "<" => "<", "=" => "=");

   package Path_Count_Map is new SPARK.Containers.Formal.Ordered_Maps (
      Key_Type => ID_Num_Range, Element_Type => Nat_64,
      "<" => "<", "=" => "=");

   package Visit_Map is new SPARK.Containers.Formal.Ordered_Maps (
      Key_Type => ID_Num_Range, Element_Type => Boolean,
      "<" => "<", "=" => "=");

   Data_File : File_Type;
   ID_Lookup : ID_Map.Map (700);
   Max_ID : ID_Num_Range;
   ID : ID_Type;
   ID_Num : ID_Num_Range;
   Parent_Num : ID_Num_Range;
   Path_Counts : Path_Count_Map.Map (700);
   Visits_FFT : Visit_Map.Map (700);
   Visits_1 : Visit_Map.Map (700);
   Visits_2 : Visit_Map.Map (700);
   Is_Valid : Boolean;
   Connections : Connection_Array_Type;
   Ch : Character;
   Count : Nat_64;
   First_ID : ID_Num_Range;
   Second_ID : ID_Num_Range;
   DAC_Visits_FFT : Boolean;

   procedure Read_ID (Data_File : File_Type;
      ID : out ID_Type; Valid : out Boolean)
   is
      Ch : Character;
   begin

      Valid := True;
      ID := [others => ' '];

      for I in 1 .. 3 loop
         if not End_Of_Line (Data_File) then
            Get (Data_File, Ch);
            ID (I) := Ch;
         else
            Valid := False;
            exit;
         end if;
      end loop;
   end Read_ID;

   procedure Get_Path_Count (ID : ID_Num_Range;
      Path_Counts : in out Path_Count_Map.Map;
      Connections : Connection_Array_Type;
      Count : out Nat_64)
   is
      Next_Count : Nat_64;
   begin
      if Path_Count_Map.Contains (Path_Counts, ID) then
         Count := Path_Count_Map.Element (Path_Counts, ID);
      else
         Count := 0;
         for I in 1 .. Connections (ID).Num_Connections loop
            Get_Path_Count (Connections (ID).Connections (I),
               Path_Counts, Connections, Next_Count);
            if Count < Nat_64'Last - Next_Count then
               Count := Count + Next_Count;
            end if;
         end loop;
         if Path_Count_Map.Length (Path_Counts) <
            Path_Counts.Capacity and then
            not Path_Count_Map.Contains (Path_Counts, ID)
         then
            Path_Count_Map.Insert (Path_Counts, ID, Count);
         end if;
      end if;
   end Get_Path_Count;

   procedure Get_Visits (ID : ID_Num_Range;
      Visits : in out Visit_Map.Map;
      Visit_Target : ID_Num_Range;
      Connections : Connection_Array_Type;
      Does_Visit : out Boolean)
   is
   begin
      Does_Visit := False;
      if Visit_Map.Contains (Visits, ID) then
         Does_Visit := Visit_Map.Element (Visits, ID);
      elsif ID = Visit_Target then
         Does_Visit := True;
      else
         for I in 1 .. Connections (ID).Num_Connections loop
            Get_Visits (Connections (ID).Connections (I),
               Visits, Visit_Target, Connections, Does_Visit);
            exit when Does_Visit;
         end loop;
         if Visit_Map.Length (Visits) < Visits.Capacity and then
            not Visit_Map.Contains (Visits, ID)
         then
            Visit_Map.Insert (Visits, ID, Does_Visit);
         end if;
      end if;
   end Get_Visits;

   procedure Get_Path_Count_B (ID : ID_Num_Range;
      Visit_Target : ID_Num_Range;
      Visits : in out Visit_Map.Map;
      Path_Counts : in out Path_Count_Map.Map;
      Connections : Connection_Array_Type;
      Count : out Nat_64)
   is
      Next_Count : Nat_64;
      Does_Visit : Boolean;
   begin
      if Path_Count_Map.Contains (Path_Counts, ID) then
         Count := Path_Count_Map.Element (Path_Counts, ID);
      else
         Count := 0;
         for I in 1 .. Connections (ID).Num_Connections loop
            Get_Visits (Connections (ID).Connections (I),
               Visits, Visit_Target, Connections, Does_Visit);
            if Does_Visit then
               Get_Path_Count_B (Connections (ID).Connections (I),
                  Visit_Target, Visits,
                  Path_Counts, Connections, Next_Count);
               if Count < Nat_64'Last - Next_Count then
                  Count := Count + Next_Count;
               end if;
            end if;
         end loop;
         if Path_Count_Map.Length (Path_Counts) <
            Path_Counts.Capacity and then
            not Path_Count_Map.Contains (Path_Counts, ID)
         then
            Path_Count_Map.Insert (Path_Counts, ID, Count);
         end if;
      end if;
   end Get_Path_Count_B;

begin
   Connections := [others => (0, [others => 0])];
   Max_ID := 0;

   ID := "you";
   if ID_Map.Length (ID_Lookup) < ID_Lookup.Capacity and then
      not ID_Map.Contains (ID_Lookup, ID)
   then
      ID_Map.Insert (ID_Lookup, ID, Max_ID);
      if Max_ID < ID_Num_Range'Last then
         Max_ID := Max_ID + 1;
      end if;
   end if;

   ID := "out";
   if ID_Map.Length (ID_Lookup) < ID_Lookup.Capacity and then
      not ID_Map.Contains (ID_Lookup, ID)
   then
      ID_Map.Insert (ID_Lookup, ID, Max_ID);
      if Max_ID < ID_Num_Range'Last then
         Max_ID := Max_ID + 1;
      end if;
   end if;

   ID := "svr";
   if ID_Map.Length (ID_Lookup) < ID_Lookup.Capacity and then
      not ID_Map.Contains (ID_Lookup, ID)
   then
      ID_Map.Insert (ID_Lookup, ID, Max_ID);
      if Max_ID < ID_Num_Range'Last then
         Max_ID := Max_ID + 1;
      end if;
   end if;

   ID := "dac";
   if ID_Map.Length (ID_Lookup) < ID_Lookup.Capacity and then
      not ID_Map.Contains (ID_Lookup, ID)
   then
      ID_Map.Insert (ID_Lookup, ID, Max_ID);
      if Max_ID < ID_Num_Range'Last then
         Max_ID := Max_ID + 1;
      end if;
   end if;

   ID := "fft";
   if ID_Map.Length (ID_Lookup) < ID_Lookup.Capacity and then
      not ID_Map.Contains (ID_Lookup, ID)
   then
      ID_Map.Insert (ID_Lookup, ID, Max_ID);
      if Max_ID < ID_Num_Range'Last then
         Max_ID := Max_ID + 1;
      end if;
   end if;

   if Path_Count_Map.Length (Path_Counts) < Path_Counts.Capacity
      and then not Path_Count_Map.Contains (Path_Counts, 1)
   then
      Path_Count_Map.Insert (Path_Counts, 1, 1);
   end if;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day11.txt");

   while not End_Of_File (Data_File) loop
      if not End_Of_Line (Data_File) then
         Read_ID (Data_File, ID, Is_Valid);

         exit when not Is_Valid;

         if ID_Map.Length (ID_Lookup) < ID_Lookup.Capacity and then
            not ID_Map.Contains (ID_Lookup, ID)
         then
            ID_Map.Insert (ID_Lookup, ID, Max_ID);
            if Max_ID < ID_Num_Range'Last then
               Max_ID := Max_ID + 1;
            end if;
         end if;
         Skip_Line (Data_File);
      end if;
   end loop;

   Close (Data_File);
   if Is_Open (Data_File) then
      Put_Line ("File did not close properly");
      return;
   end if;

   Open (File => Data_File,
         Mode => In_File,
         Name => "data/day11.txt");

   while not End_Of_File (Data_File) loop
      if not End_Of_Line (Data_File) then
         Read_ID (Data_File, ID, Is_Valid);

         exit when not Is_Valid;

         exit when End_Of_Line (Data_File);
         Get (Data_File, Ch);
         if Ch /= ':' then
            Put_Line ("Expected colon");
         end if;

         exit when End_Of_Line (Data_File);
         Get (Data_File, Ch);
         if Ch /= ' ' then
            Put_Line ("Expected space");
         end if;

         if ID_Map.Contains (ID_Lookup, ID) then
            Parent_Num := ID_Map.Element (ID_Lookup, ID);

            while not End_Of_Line (Data_File) loop
               Read_ID (Data_File, ID, Is_Valid);
               exit when not Is_Valid;

               if ID_Map.Contains (ID_Lookup, ID) then
                  ID_Num := ID_Map.Element (ID_Lookup, ID);

                  if Connections (Parent_Num).Num_Connections <
                     Connection_List_Type'Last
                  then
                     Connections (Parent_Num).Num_Connections :=
                        Connections (Parent_Num).Num_Connections + 1;
                     Connections (Parent_Num).Connections (
                        Connections (Parent_Num).Num_Connections) :=
                           ID_Num;
                  end if;
               else
                  Put ("Invalid ID ");
                  Put (ID (1));
                  Put (ID (2));
                  Put (ID (3));
                  New_Line;
               end if;

               if not End_Of_Line (Data_File) then
                  Get (Data_File, Ch);
                  if Ch /= ' ' then
                     Put_Line ("Expected space");
                  end if;
               end if;
            end loop;
         end if;
         Skip_Line (Data_File);
      end if;
   end loop;

   Get_Path_Count (0, Path_Counts, Connections, Count);
   Put ("Day 11a = ");
   Put (Count, Width => 0);
   New_Line;

   Get_Visits (3, Visits_FFT, 4, Connections, DAC_Visits_FFT);

   if DAC_Visits_FFT then
      First_ID := 4;
      Second_ID := 3;
   else
      First_ID := 3;
      Second_ID := 4;
   end if;

   Path_Count_Map.Clear (Path_Counts);

   --  Initialize Path_Counts with a count of 1 to out
   if Path_Count_Map.Length (Path_Counts) < Path_Counts.Capacity
      and then not Path_Count_Map.Contains (Path_Counts, 1)
   then
      Path_Count_Map.Insert (Path_Counts, 1, 1);
   end if;

   --  Find the path count from First_ID to out
   Get_Path_Count (First_ID, Path_Counts, Connections, Count);

   Path_Count_Map.Clear (Path_Counts);

   --  Initialize Path_Counts with a path count of Count to First_ID
   if Path_Count_Map.Length (Path_Counts) < Path_Counts.Capacity
      and then not Path_Count_Map.Contains (Path_Counts, First_ID)
   then
      Path_Count_Map.Insert (Path_Counts, First_ID, Count);
   end if;

   --  Find the path count from Second_ID to First_ID
   Get_Path_Count_B (Second_ID, First_ID, Visits_1,
      Path_Counts, Connections, Count);

   Path_Count_Map.Clear (Path_Counts);

   --  Initialize Path_Counts with a path count of Count to Second_ID
   if Path_Count_Map.Length (Path_Counts) < Path_Counts.Capacity
      and then not Path_Count_Map.Contains (Path_Counts, Second_ID)
   then
      Path_Count_Map.Insert (Path_Counts, Second_ID, Count);
   end if;

   --  Count the paths from svr to Second_ID
   Get_Path_Count_B (2, Second_ID, Visits_2,
      Path_Counts, Connections, Count);

   Put ("Day 11b = ");
   Put (Count, Width => 0);
   New_Line;

exception
   when others => Put_Line ("Exception");
end Day11;
