with Ada.Strings.Unbounded.Text_IO;
with Ada.Assertions;
with Ada.Text_IO;

package body Digamma.CSV is

   use Ada.Assertions;

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

   function End_Of_Line (Item : Unbounded_String; P : Natural) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      return P > Length (Item);
   end;

   function Get (Item : Unbounded_String; Pattern : String; P : in out Natural) return String is
      use Ada.Strings.Unbounded;
      High : Natural;
      Low : Positive := P + 1;
   begin
      High := Index (Item, Pattern, Low);
      if High = 0 then
         High := Length (Item) + 1;
      end if;
      P := High;
      --Assert (High - 1 >= 0);
      Ada.Text_IO.Put (Low'Img & ", " & High'Img & "   ");
      return Slice (Item, Low, High - 1);
   end;


   function Get (Item : Unbounded_String; Pattern : String; P : in out Natural) return Integer is
   begin
      return Integer'Value (Get (Item, Pattern, P));
   end;

   procedure Read (Name : String; To : in out Line_Vector) is
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
      Read (Name, R);
      return R;
   end;

end;
