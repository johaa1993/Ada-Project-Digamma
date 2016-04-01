with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Unbounded;
with System;
with Digamma.CSV;
with Digamma;
with Ada.Strings.Fixed;

procedure Main_Test is

   use Digamma;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Ada.Integer_Text_IO;
   use Ada.Strings.Fixed;

   L : CSV.Line_Vector;

   procedure Test (Item : CSV.Unbounded_String) is
      P : Natural := 0;
   begin
      while not CSV.End_Of_Line (Item, P) loop
         declare
            S : String := CSV.Get (Item, ",", P);
         begin
            Put (Head (S, 20));
            Put (" ");
            --Put (Integer'Value (S), 3);
            Put (" ");
            Put (CSV.End_Of_Line (Item, P)'Img);
            New_Line;
         end;
      end loop;
   end;

begin

   CSV.Read ("test.csv", L);

   for E of L loop
      Put_Line (To_String (E));
      Test (E);
   end loop;


end;
