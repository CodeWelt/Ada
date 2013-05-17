--  FILE:    LCM.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 2
--  VERSION: 1.0
--  DATE:    10.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 2.3: Kleinstes gemeinsames Vielfaches
--
--  Der Benutzer gibt zwei positive ganze Zahlen ein.
--  Das Programm berechnet das kleinste gemeinsame Vielfache
--  dieser zwei Zahlen und gibt es aus.
--
-------------------------------------------------------------------

WITH Ada.Text_Io, Ada.Integer_Text_Io;
USE  Ada.Text_Io, Ada.Integer_Text_Io;

PROCEDURE LCM IS

   --  FUNCTION GgT
   --  Die rekursive Funktion berechnet den größten gemeinsamen
   --  Teiler der vom Benutzer eingegebenen zwei Zahlen.
   --
   --  PARAMETERS:
   --  Die Integer Parameter ErsteZahl und ZweiteZahl wurden
   --  vom Benutzer eingegeben und an die Funktion übergeben.
   --
   --  RETURNS: Die Funktion gibt den größten gmeinsamen
   --  Teiler der beiden übergebenen Zahlen als Integer zurück.
   FUNCTION GgT (ErsteZahl, ZweiteZahl: Integer) RETURN Integer IS
   BEGIN      
      IF ErsteZahl = ZweiteZahl THEN         
         RETURN ErsteZahl;         
      ELSIF ErsteZahl > ZweiteZahl THEN         
         RETURN GgT(ErsteZahl - ZweiteZahl, ZweiteZahl);         
      ELSE          
         RETURN GgT(ZweiteZahl - ErsteZahl, ErsteZahl);         
      END IF;      
   END GgT;
   
   ErsteZahl: Integer := 0;
   ZweiteZahl: Integer := 0;

BEGIN
   Ada.Text_IO.Put ("erste Zahl:  ");
   Get (ErsteZahl);
   
   Ada.Text_IO.Put ("zweite Zahl: ");
   Get (ZweiteZahl);

   Ada.Text_IO.Put ("kgV:");
   -- Das kleinste gemeinsame Vielfache ist die erste Zahl multipliziert
   -- mit der Zweiten geteilt durch den größten gemeinsamen Teiler
   -- dieser zwei Zahlen. Beispiel: (a * b) / GgT(a, b)
   Put(Integer'Image( (ErsteZahl * ZweiteZahl) / GgT(ErsteZahl, ZweiteZahl) ));
   
END LCM;
