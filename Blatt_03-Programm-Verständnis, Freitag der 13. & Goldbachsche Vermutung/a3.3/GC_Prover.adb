--  FILE:    GC_Prover.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 3
--  VERSION: 1.0
--  DATE:    17.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 3.3: Goldbachsche Vermutung
--
--  Die Vermutung ist dass jede gerade Zahl größer als 2 als
--  Summe zweier Primzahlen geschrieben werden kann.
--  Das Programm überprüft ob diese Vermutung in einem
--  bestimmten Bereich zutrifft oder nicht.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Integer_Text_IO, Goldbach;
use  Ada.Text_IO, Ada.Integer_Text_IO, Goldbach;

procedure GC_Prover
is
   Min : Integer := 0;
   Max : Integer := 0;
   Firstx : Prime_Number;
   Secondx : Prime_Number;
   Foundx : Boolean := False;
begin

   Ada.Text_IO.Put ("Goldbachsche Vermutung beweisen");
   New_Line;
   
   Ada.Text_IO.Put ("von: ");
   Get (Min);
   
   Ada.Text_IO.Put ("bis: ");
   Get (Max);

   --  Es wird ein Feld Prim des Typs Prime_Field in geeigneter Größe erzeugt.
   --  Es werden zunächst alle Komponenten des Felds auf True gesetzt.
   declare
      Prim : Prime_Field (2 .. Max) := (others => True);
   begin
      
      --  Der Algorithmus Sieb des Eratosthenes wird verwendet um die
      --  Komponenten des Felds Prim an jeder Index-Position, die eine
      --  Primzahl ist, den Wert True zu geben, an allen anderen den
      --  Wert False.
      Eratosthenes (Prim);
      --  Die Schleife läuft für den vom Benutzer eingegebenen Bereich.
      for Laufvar in Min .. Max loop
         --  Nur für gerade Zahlen größer als 2 wird Prove_Conjecture
         --  aufgerufen.
         if (Laufvar mod 2) /= 0 then
            delay 0.01;
         else
            Prove_Conjecture (Laufvar, Prim, Firstx, Secondx, Foundx);
            --  Falls zwei Primzahlen gefunden wurden, werden diese ausgegeben.
            if Foundx = True then
               New_Line;
               Put (Integer'Image (Laufvar) & " =" & Integer'Image (Firstx)
               & " +" & Integer'Image (Secondx));
            else
               Put (Integer'Image (Laufvar) & ": G.V. trifft nicht zu.");
            end if;
         end if;
      end loop;

   end;

end GC_Prover;
