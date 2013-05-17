--  FILE:    Encode.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 6
--  VERSION: 1.0
--  DATE:    09.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 6.1: Ausnahmen
--
--  Ein Paar natürlicher Zahlen a, b kann in einer Zahl
--  kodiert werden durch die Funktion
--  f(a, b) := (2 ** a) * (3 ** b).
--  Das Programm berechnet die kodierung f(a, b) aus.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO;

procedure Encode is

   ZahlA, ZahlB : Natural := 1;

begin
   Put ("Aufgabe 6.1: Ausnahmen");
   New_Line;
   Put ("Bitte geben Sie eine natürliche Zahl (A) ein: ");
   Get (ZahlA);
   Put ("Bitte geben Sie eine natürliche Zahl (B) ein: ");
   Get (ZahlB);
   --  Das Ergebnis wird berechnet und ausgegeben.
   Put ("Encoded: (2 **" & ZahlA'Img & ") * (3 **" & ZahlB'Img & ") = "
    & Integer'Image ((2 ** ZahlA) * (3 ** ZahlB)));
   New_Line;

exception
   --  Exception Handler
   when Constraint_Error =>
      Put ("Kodierung kann nicht berechnet werden.");
      New_Line;
      Put ("Das Programm wird neu gestartet.");
      New_Line;
      New_Line;
      Encode;
   when Data_Error =>
      Put ("Kodierung kann nicht berechnet werden."); 
      New_Line;
      Put ("Das Programm wird neu gestartet.");
      New_Line;
      New_Line;
      Encode;      
end Encode;
