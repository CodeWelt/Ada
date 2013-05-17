--  FILE:    PiBerech.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 5
--  VERSION: 1.0
--  DATE:    02.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 5.1: Berechnung von Pi
--
--  Die Zahl Pi bezeichnet die Fläche des Einheitskreises.
--  Mit Hilfe eines Zufallsexperiments kann die Zahl Pi
--  geschätzt werden. Hierzu wählt das Programm n-mal einen
--  zufälligen Punkt innerhalb des Quadrats 1 * 1 und zählt
--  mit Hilfe des Satz des Pytagoras, wie oft dieser Punkt
--  innerhalb des Einheitskreises liegt, das ist s-mal der Fall.
--  Daraus ergibt sich die Formel:
--  Pi := 4 * (s / n);
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Numerics.Float_Random, Ada.Float_Text_IO,
     Ada.Numerics.Elementary_Functions, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Numerics.Float_Random, Ada.Float_Text_IO,
     Ada.Numerics.Elementary_Functions;

procedure PiBerech is
   Gen : Generator;
   x : Float;
   y : Float range 0.0 .. 1.0;
   CountTreffer : Integer := 0;
   nmal : Natural := 1000;
   Pi : Float := 0.00;
begin
   Ada.Numerics.Float_Random.Reset (Gen);

   Put ("Zufallsexperiment zur Berechnung von Pi v1.0");
   New_Line;
   Put ("Bitte geben Sie die Anzahl der Punkte ein: ");
   Ada.Integer_Text_IO.Get (nmal);

   --  Die Schleife läuft von 1 bis zur Anzahl der vom
   --  Benutzer eingegebenen Punkte.
   for Laufvar in 1 .. nmal loop
      --  Die Koordinaten werden zufällig gewählt.
      x := Random (Gen);
      y := Random (Gen);
      
      --  Liegt der gerade zufällig gewählte Punkt innerhalb
      --  des Einheitskreises, so wird der Punkt als Treffer
      --  gezählt.
      if Sqrt (x ** 2 + y ** 2) <= 1.00 then
         CountTreffer := CountTreffer + 1;
      end if;
      
      --  Nach je 1.000 Punkten wird die aktuelle
      --  Näherung an Pi berechnet und ausgegeben.
      if Laufvar mod 1000 = 0 then
         Pi := (Float (4 * CountTreffer) / Float (Laufvar));
         Put ("Näherungswert für Pi ist: ");
         Ada.Float_Text_IO.Put (Pi, 0, 17, 0);
         New_Line;
      end if;
   end loop;

   exception
      when Constraint_Error =>
         Put ("Constraint_Error: Bitte geben Sie eine positive Zahl an.");
         New_Line;
         Put ("Das Experiment wird neu gestartet.");
         New_Line;
         PiBerech;

end PiBerech;
