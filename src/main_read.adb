with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;

with System;

with Digamma;
with Digamma.CSV;

procedure Main_Read is

   use Digamma;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Strings.Fixed;
   use Ada.Float_Text_IO;
   use Ada.Streams;
   use Ada.Streams.Stream_IO;

   M : CSV.Float_Matrix (3, 4);

   F : Stream_IO.File_Type;
   S : Stream_Access;

begin

   Open (F, In_File, "test.advec");
   S := Stream (F);
   CSV.Float_Vector'Read (S, M.Data);
   Close (F);

   for I in 1 .. M.Row_Count loop
      for J in 1 .. M.Column_Count loop
         Put (CSV.Element (M, I, J));
         Put (" ");
      end loop;
      New_Line;
   end loop;


end;
