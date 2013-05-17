--  FILE:    Decode.adb
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
--  Das Programm berechnet das ursprüngliche Zahlenpaar aus
--  einer kodierten Zahl.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO;

procedure Decode is

   ZahlA, ZahlAx : Natural := 0;
   Anzahldurch2, Anzahldurch3 : Integer := 0;
begin

   Put ("Aufgabe 6.1: Ausnahmen");
   New_Line;   
   Put ("Bitte geben Sie eine natürliche Zahl ein,");
   New_Line;
   Put ("die decodet werden soll: ");
   Get (ZahlA);
   ZahlAx := ZahlA;
   
   --  Die Schleife läuft solange bis die Zahl nichtmehr durch
   --  2 teilbar ist. Die Anzahl wie oft durch 2 geteilt wurde
   --  wird in Anzahldurch2 gespeichert.
   while ZahlA mod 2 = 0 and ZahlA > 0 loop
      Anzahldurch2 := Anzahldurch2 + 1;
      ZahlA := ZahlA / 2;
   end loop;

   --  Die Schleife läuft solange bis die Zahl nichtmehr durch
   --  3 teilbar ist. Die Anzahl wie oft durch 3 geteilt wurde
   --  wird in Anzahldurch3 gespeichert.
   while ZahlA mod 3 = 0 and ZahlA > 0 loop
      Anzahldurch3 := Anzahldurch3 + 1;
      ZahlA := ZahlA / 3;
   end loop;

   --  Nur wenn nach den zwei while Schleifen eine 1 übrig bleit
   --  wurde die Zahl vollständig decodiert.
   if ZahlA = 1 then
      Put ("Decoded: " & ZahlAx'Img & " = (2 ** " & Anzahldurch2'Img
       & ") * (3 ** " & Anzahldurch3'Img & ")");
   else
      Put ("Die angegebene Zahl kann nicht decodiert werden.");
   end if;
   
exception
   --  Exception Handler
   when Constraint_Error =>
      Put ("Kodierung kann nicht berechnet werden.");
      New_Line;
      Put ("Das Programm wird neu gestartet.");
      New_Line;
      New_Line;
      Decode;
   when Data_Error =>
      Put ("Kodierung kann nicht berechnet werden."); 
      New_Line;
      Put ("Das Programm wird neu gestartet.");
      New_Line;
      New_Line;
      Decode;    

end Decode;