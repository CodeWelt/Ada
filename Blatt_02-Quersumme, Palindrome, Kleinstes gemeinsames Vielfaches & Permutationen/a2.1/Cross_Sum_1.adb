--  FILE:    Cross_Sum_1.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 2
--  VERSION: 1.0
--  DATE:    10.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 2.1: Quersumme
--
--  Die Quersumme einer Zahl ist die Summe ihrer Ziffern bei
--  Darstellung im Dezimalsystem.
--  Das Programm berechnet die Quersumme der vom Benutzer
--  eingegebenen Zahl mit Hilfe einer While Schleife.
--
-------------------------------------------------------------------

WITH Ada.Text_Io, Ada.Integer_Text_Io;
USE  Ada.Text_Io, Ada.Integer_Text_Io;

PROCEDURE Cross_Sum_1 IS

   Eingabezahl : Integer := 0;
   Weiter : Integer := 0;
   Weiter2 : Integer := 0;
   Temp2 : Integer := 0;
   Ergebnis : Integer := 0;
BEGIN
   Ada.Text_IO.Put ("Eingabezahl: ");
   Get (Eingabezahl);
   
   -- Falls eine negative Eingabezahl eingegeben wurde wird diese mit -1 multipliziert.
   IF Eingabezahl < 0 THEN
      Weiter := Eingabezahl * (-1);
   ELSE
      Weiter := Eingabezahl;
   END IF;
   
   WHILE Weiter /= 0 LOOP            
      Temp2 :=  Weiter / 10;   -- Wert wird ganzzahlig geteilt.      
      Weiter2 := Weiter mod 10;   -- Rest wird ermittelt.      
      
      Ergebnis := Ergebnis + Weiter2;
      Weiter := Temp2;
   END LOOP;

   Ada.Text_IO.Put ("Quersumme: ");
   Ada.Text_IO.Put (Integer'Image(Ergebnis));
   
END Cross_Sum_1;
