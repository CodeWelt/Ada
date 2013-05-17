-------------------------------------------------------------------
--
--  FILE:    muenz.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 1
--  VERSION: 1.0
--  DATE:    03.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 1.2: Muenzauswahl
--
--  Die eingegebenen Münzen werden mit Hilfe einer Schleife 
--  eingelesen und gespeichert.
--  Diese Daten werden an Denominations.Split_Greedy und
--  Denominations.Put_Usage als Parameter übergeben.
--
-------------------------------------------------------------------

WITH Ada.Text_Io, Ada.Integer_Text_Io, Denominations;
USE  Ada.Text_Io, Ada.Integer_Text_Io, Denominations;

PROCEDURE Muenz
IS
   Coins : Denominations.Denomination (1..300);
   Money : Denominations.Money_Amount;
   Laufvar : Integer;
   
   Storeme : Integer := 0;
BEGIN
   
   FOR Loopme IN 1..300 LOOP
      Ada.Text_IO.Put ("Münze" & Integer'Image(Loopme) & ": ");
      Ada.Integer_Text_IO.Get (Storeme);
      IF Storeme = 0 THEN         -- Die Eingabe der Münzen wird durch eine Null beendet.
         Laufvar := Loopme -1;
         EXIT;
      END IF;
      
      IF Loopme = 300 THEN         -- Prüfung der Obergrenze von 300 Münzen.
         Ada.Text_IO.Put ("Maximale Anzahl Münzen erreicht.");
         New_Line;
         EXIT;
      END IF;
      
      IF Loopme > 1 THEN         -- Wenn die vorrangegangene Zahl größer ist als die zu speichernde Zahl wird eine Warnung ausgegeben.  
         IF Coins(Loopme-1) > Storeme THEN
            Put ("Warnung: Eingabe ist nicht aufsteigend sortiert. Ergebnisse werden falsch sein.");
            New_Line;
         END IF;
      END IF;
      
      Coins(Loopme) := Storeme;
   END LOOP;
   
   Ada.Text_IO.Put ("Geldbetrag: ");
   Ada.Integer_Text_IO.Get(Money);
   
   Ada.Text_IO.Put ("Ergebnis b) ");
   -- Die Procedure Put_Usage und die Funktion Split_Greedy aus dem Packet denominations
   -- werden mit den dazugehörigen Parametern aufgerufen.
   Denominations.Put_Usage(Denominations.Split_Greedy(Money, Coins(1..Laufvar)), Coins);
   
   New_Line;
   Ada.Text_IO.Put ("Ergebnis c) ");
   
END Muenz;

