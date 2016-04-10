with Ada.Strings.Unbounded.Text_IO;
with Ada.Assertions;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

package body Digamma.CSV is

   use Ada.Assertions;
   use Ada.Strings;
   use Ada.Strings.Fixed;

   procedure Append (F : File_Type; Container : in out Line_Vector) is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded.Text_IO;
   begin
      loop
         exit when End_Of_File (F);
         Container.Append (Get_Line (F));
      end loop;
   end;

   procedure Get (F : File_Type; Container : in out Line_Vector) is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded.Text_IO;
   begin
      for E of Container loop
         exit when End_Of_File (F);
         E := Get_Line (F);
      end loop;
   end;

   procedure Read_Append (Name : String; To : in out Line_Vector) is
      use Ada.Text_IO;
      F : File_Type;
   begin
      Open (F, In_File, Name);
      Append (F, To);
      Close (F);
   end;

   function Read (Name : String) return Line_Vector is
      R : Line_Vector;
   begin
      Read_Append (Name, R);
      return R;
   end;

   procedure Next (Item : Unbounded_String; Pattern : String; Low : out Positive; P : in out Natural) is
      use Ada.Strings.Unbounded;
      use Ada.Strings;
   begin
      Low := P + 1;
      P := Index (Item, Pattern, Low, Forward);
      if P = 0 then
         P := Length (Item) + 1;
      end if;
   end;

   function End_Of_Line (Item : Unbounded_String; P : Natural) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      return P > Length (Item);
   end;


   function Get_Count (Item : Unbounded_String; Pattern : String) return Natural is
      Low : Natural;
      P : Natural := 0;
      C : Natural := 0;
   begin
      loop
         exit when End_Of_Line (Item, P);
         Next (Item, Pattern, Low, P);
         C := C + 1;
      end loop;
      return C;
   end;

   procedure Get_Count (Item : Line_Vector; Pattern : String; To : in out Column_Count_Vector) is
   begin
      for E of Item loop
         To.Append (Get_Count (E, Pattern));
      end loop;
   end;

   procedure Get_Count (Item : Line_Vector; Pattern : String; To : in out Column_Count_Vector; Min : in out Natural; Max : in out Natural) is
   begin
      for E of Item loop
         To.Append (Get_Count (E, Pattern));
         Min := Natural'Min (Min, To.Last_Element);
         Max := Natural'Max (Max, To.Last_Element);
      end loop;
   end;

   function Element (Item : Float_Matrix; Row, Column : Positive) return Float is
      I : Natural := (Row - 1) * Item.Column_Count + Column;
   begin
      Assert (Row <= Item.Row_Count, "Row = " & Row'Img & " is out side range. Max is " & Item.Row_Count'Img & ".");
      Assert (Column <= Item.Column_Count, "Column " & Column'Img & " is out side range. Max is " & Item.Column_Count'Img & ".");
      Assert (I <= Item.Data.Last_Index, "I <= Item.Data.Last_Index");
      return Item.Data.Element (I);
   end;

   procedure Set (Item : in out Float_Matrix; Row, Column : Positive; Value : Float) is
      I : Natural := (Row - 1) * Item.Column_Count + Column;
   begin
      Assert (Row <= Item.Row_Count, "Row = " & Row'Img & " is out side range. Max is " & Item.Row_Count'Img & ".");
      Assert (Column <= Item.Column_Count, "Column " & Column'Img & " is out side range. Max is " & Item.Column_Count'Img & ".");
      Assert (I <= Item.Data.Last_Index, "I <= Item.Data.Last_Index");
      Item.Data (I) := Value;
   end;

   procedure Append (Item : Line_Vector; Pattern : String; To : in out Float_Matrix) is
      use Ada.Strings.Unbounded;
      Low : Natural;
      P : Natural;
   begin
      for I in Item.First_Index .. Item.Last_Index loop
         P := 0;
         loop
            exit when End_Of_Line (Item.Element (I), P);
            Next (Item.Element (I), Pattern, Low, P);
            declare
               S : String := Slice (Item.Element (I), Low, P - 1);
            begin
               To.Data.Append (Float'Value (S));
            exception
               when others =>
                  P := P - 1;
                  raise Program_Error with "The string value (" & S & ") on line " & I'Img & " column " & Low'Img & " .. " & P'Img & " can not be converted to float.";
            end;
         end loop;
      end loop;
   end;



   procedure Append (Item : Line_Vector; Pattern : String; To : in out Float_Array_2) is
      use Ada.Strings.Unbounded;
      Low : Natural;
      P : Natural;
      Row : Integer range To'Range (1) := To'First (1);
      Column : Integer range To'Range (2);
   begin




      for I in Item.First_Index .. Item.Last_Index loop
         P := 0;
         Column := To'First (2);
         loop
            exit when End_Of_Line (Item.Element (I), P);
            Next (Item.Element (I), Pattern, Low, P);
            declare
               S : String := Slice (Item.Element (I), Low, P - 1);
            begin
               Ada.Text_IO.Put_Line (S);
               Assert (Row <= To'Last (1), "(Row = "&Trim (Row'Img, Both)&") <= (Last = "&Trim (To'Last (1)'Img, Both)&")");
               Assert (Column <= To'Last (2), "(Column = "&Trim (Column'Img, Both)&") <= (Last = "&Trim (To'Last (2)'Img, Both)&")");
               To (Row, Column) := Float'Value (S);
               Column := Column + 1;
            --exception
               --when others =>
                  --P := P - 1;
                  --raise Program_Error with "The string value (" & S & ") on line " & I'Img & " column " & Low'Img & " .. " & P'Img & " can not be converted to float.";
            end;
         end loop;
         Row := Row + 1;
      end loop;
   end;


   procedure Assert_Column_Count (Item : Line_Vector; Pattern : String; Column_Count : Natural; Message : String := "") is
      N : Natural;
   begin
      for I in Item.First_Index + 1 .. Item.Last_Index loop
         N := Get_Count (Item (I), Pattern);
         Assert (Column_Count = N, "Column count at line " & I'Img & " is not " & N'Img & ".");
      end loop;
   end;

   procedure Assert_Row_Column_Equality (Item : Line_Vector; Pattern : String; Rectangle : Float_Matrix; Message : String := "") is
      --use type Ada.Containers.Count_Type;
   begin
      Assert (Natural (Item.Length) = Rectangle.Row_Count, "Row count " & Item.Length'Img & " is not " & Rectangle.Row_Count'Img & ".");
      Assert_Column_Count (Item, Pattern, Rectangle.Column_Count);
   end;

end;
