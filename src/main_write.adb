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

   L : CSV.Line_Vector;
   M : CSV.Float_Matrix (3, 4);

   F : Stream_IO.File_Type;
   S : Stream_Access;

begin

   Create (F, Out_File, "test.advec");
   S := Stream (F);

   CSV.Read_Append ("test.csv", L);
   CSV.Assert_Row_Column_Equality (L, ",", M);
   CSV.Append (L, ",", M);
   CSV.Float_Vector'Write (S, M.Data);
   Close (F);

   for I in 1 .. M.Row_Count loop
      for J in 1 .. M.Column_Count loop
         Put (CSV.Element (M, I, J));
         Put (" ");
      end loop;
      New_Line;
   end loop;


end;
