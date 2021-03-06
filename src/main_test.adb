with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;

with System;

with Digamma;
with Digamma.CSV;



procedure Main_Test is

   use Digamma;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Float_Text_IO;
   use Ada.Streams;
   use Ada.Streams.Stream_IO;

   X : CSV.Float_Array_2 (1 .. 3, 1 .. 4);
   L : CSV.Line_Vector;

begin

   CSV.Read_Append ("test.csv", L);
   CSV.Append (L, ",", X);

   for I in X'Range (1) loop
      for J in X'Range (2) loop
         Put (X (I, J));
         Put (" ");
      end loop;
      New_Line;
   end loop;


   null;

end;
