with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package Digamma.CSV is

   package Line_Vectors is new Ada.Containers.Vectors (Positive, Ada.Strings.Unbounded.Unbounded_String, Ada.Strings.Unbounded."=");
   package Column_Count_Vectors is new Ada.Containers.Vectors (Positive, Natural);
   package Float_Vectors is new Ada.Containers.Vectors (Positive, Float);

   subtype Line_Vector is Line_Vectors.Vector;
   subtype Column_Count_Vector is Column_Count_Vectors.Vector;
   subtype File_Type is Ada.Text_IO.File_Type;
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
   subtype Float_Vector is Float_Vectors.Vector;

   type Float_Array_2 is array (Integer range <>, Integer range <>) of Float;

   procedure Append (F : File_Type; Container : in out Line_Vector);
   procedure Get (F : File_Type; Container : in out Line_Vector);

   procedure Read_Append (Name : String; To : in out Line_Vector);
   function Read (Name : String) return Line_Vector;

   --function Get (Item : Unbounded_String; Pattern : String; P : in out Natural) return String;

   function End_Of_Line (Item : Unbounded_String; P : Natural) return Boolean;

   procedure Get_Count (Item : Line_Vector; Pattern : String; To : in out Column_Count_Vector);


   type Float_Matrix (Row_Count : Natural; Column_Count : Natural) is record
      Data : Float_Vector;
   end record;

   function Element (Item : Float_Matrix; Row, Column : Positive) return Float;
   procedure Set (Item : in out Float_Matrix; Row, Column : Positive; Value : Float);

   procedure Append (Item : Line_Vector; Pattern : String; To : in out Float_Matrix);
   procedure Append (Item : Line_Vector; Pattern : String; To : in out Float_Array_2);

   procedure Assert_Column_Count (Item : Line_Vector; Pattern : String; Column_Count : Natural; Message : String := "");
   procedure Assert_Row_Column_Equality (Item : Line_Vector; Pattern : String; Rectangle : Float_Matrix; Message : String := "");

end;
