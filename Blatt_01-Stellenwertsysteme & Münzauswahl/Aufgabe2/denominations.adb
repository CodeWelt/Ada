-------------------------------------------------------------------------------
--
--  FILE:    denominations.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 1
--  VERSION: $Revision: 33 $
--  DATE:    $Date: 2006-10-25 19:57:56 +0200 (Wed, 25 Oct 2006) $
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------------------
--
--  PACKAGE BODY Denominations
--
--  Implementierung des Pakets 'Denominations'
--
--  Die Procedure Put_Usage und die Funktion Split_Greedy werden von der
--  muenz.adb mit den dazugehörigen Parametern aufgerufen.
--  Put_Usage gibt die von Split_Greedy berechneten Münzen
--  formatiert aus.
--  
-------------------------------------------------------------------------------

WITH Ada.Integer_Text_IO, Ada.Text_IO;

package body Denominations is


   -------------------------
   -- Loesungsalgorithmen --
   -------------------------


   FUNCTION Split_Greedy
     (Amount : in Money_Amount;
      Coins  : in Denomination)
     RETURN Denomination_Usage
   IS
      Betrag : Denominations.Money_Amount;
      Returnme : Denominations.Denomination_Usage(1..Coins'Last);
   BEGIN
      
      Betrag := Amount;
      
      IF Betrag = 0 THEN                     -- Wenn der Betrag gleich Null ist,
         RETURN (1..300 => 0);               -- soll von Put_Usage "Nicht darstellbar." ausgegeben werden.
      END IF;
      
      FOR Laufv IN 1..Coins'Last LOOP         -- Keine Münzen als Startwert.
         Returnme(Laufv) := 0;
      END LOOP;

      FOR Lauf IN REVERSE 1..Coins'Last LOOP         -- Schleife läuft für jede Münze (angefangen bei der Größten).
         
         IF Coins(Lauf) <= Betrag THEN
            Returnme(Lauf) := Betrag / Coins(Lauf);         -- Betrag wird ganzzahlig geteilt und das Ergebnis gespeichert.
            Betrag := Betrag mod Coins(Lauf);               -- Der Rest wird dem neuen Betrag zugewiesen.
         END IF;
         
      END LOOP;

      IF Betrag /= 0 THEN                -- Wenn der Betrag nach der Schleife nicht Null ist,
         RETURN (1..300 => 0);           -- das bedeutet es wurde für den Betrag keine passende Münz-kombination gefunden,
      end if;                            -- soll von Put_Usage "Nicht darstellbar." ausgegeben werden.

      RETURN Returnme;            -- Die verwendeten Münzen werden zurückgegeben.
      
   END Split_Greedy;


   function Split_Backtracking
     (Amount : in Money_Amount;
      Coins  : in Denomination)
     return Denomination_Usage
   is
   BEGIN
      return (1 .. 0 => 0); -- TODO: Ersetzen Sie dieses Statement
   end Split_Backtracking;


   -------------
   -- Ausgabe --
   -------------


   PROCEDURE Put_Usage
     (Usage : in Denomination_Usage;
      Coins : in Denomination)
   IS
      Hatwas : Boolean := False;
   BEGIN
      
      FOR Position IN REVERSE 1..Usage'Last LOOP         -- Die von Split_Greedy zurückgegebenen verwendeten Münzen werden,
         IF Usage(Position) /= 0 THEN                    -- angefangen bei der größten Münze, formatiert ausgegeben.
            Hatwas := True;
            
            Ada.Integer_Text_IO.Put(Usage(Position), 1);
            Ada.Text_IO.Put ("x");
            Ada.Integer_Text_IO.Put(Coins(Position), 1);
            Ada.Text_IO.Put (" ");
         END IF;
      END LOOP;
      
      IF Hatwas = False THEN         -- Falls keine Münzen zurückgegeben worden sind wird "Nicht darstellbar." ausgegeben.
         Ada.Text_IO.Put ("Nicht darstellbar.");
      END IF;
   end Put_Usage;

end Denominations;
