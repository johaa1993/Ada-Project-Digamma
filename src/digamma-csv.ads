with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package Digamma.CSV is

   package Line_Vectors is new Ada.Containers.Vectors (Natural, Ada.Strings.Unbounded.Unbounded_String, Ada.Strings.Unbounded."=");

   subtype Line_Vector is Line_Vectors.Vector;
   subtype File_Type is Ada.Text_IO.File_Type;
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   procedure Append (F : File_Type; Container : in out Line_Vector);
   procedure Get (F : File_Type; Container : in out Line_Vector);

   procedure Read (Name : String; To : in out Line_Vector);
   function Read (Name : String) return Line_Vector;

   function Get (Item : Unbounded_String; Pattern : String; P : in out Natural) return Integer;
   function Get (Item : Unbounded_String; Pattern : String; P : in out Natural) return String;

   function End_Of_Line (Item : Unbounded_String; P : Natural) return Boolean;

end;
