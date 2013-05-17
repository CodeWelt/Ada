--  FILE:    Cross_Sum_2.adb
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
--  eingegebenen Zahl mit Hilfe der rekursiven Funktion Quersumme.
--
-------------------------------------------------------------------

WITH Ada.Text_Io, Ada.Integer_Text_Io;
USE  Ada.Text_Io, Ada.Integer_Text_Io;

PROCEDURE Cross_Sum_2 IS

   --  FUNCTION Quersumme
   --  Die rekursive Funktion berechnet die Quersumme einer gegebenen
   --  positiven Zahl und gibt das Ergebnis zurück.
   --
   --  PARAMETERS:
   --  Input ist die Zahl dessen Quersumme gebildet werden soll.
   --
   --  RETURNS: Die Quersumme der als Parameter gelieferten
   --  Zahl wird als Integer zurückgegeben.
   FUNCTION Quersumme (Input: Integer) RETURN Integer IS
      Rest : Integer := 0;
   BEGIN
      Rest := Input mod 10;   -- Rest wird ermittelt.
      IF Input = 0 THEN RETURN Input;
      ELSE
         RETURN Rest + Quersumme(Input / 10);      
      END IF;  
   END;

   Eingabezahl : Integer := 0; 
BEGIN

   Ada.Text_IO.Put ("Eingabezahl: ");
   Get (Eingabezahl);
   
   -- Falls eine negative Eingabezahl eingegeben wurde wird diese mit -1 multipliziert.
   IF Eingabezahl < 0 THEN
      Eingabezahl := Eingabezahl * (-1);
   END IF;
   
   Ada.Text_IO.Put ("Quersumme: ");
   -- Der Rückgabewert von der rekursiven Funktion Quersumme wird ausgegeben.
   Ada.Text_IO.Put (Integer'Image(Quersumme(Eingabezahl)));

END Cross_Sum_2;
