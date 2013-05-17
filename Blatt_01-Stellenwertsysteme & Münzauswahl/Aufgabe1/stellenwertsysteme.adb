-------------------------------------------------------------------
--
--  FILE:    stellenwertsysteme.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 1
--  VERSION: 1.0
--  DATE:    03.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 1.1: Stellenwertsysteme
--
--  Das Programm wandelt eine Zahl in ein anderes
--  Stellenwertsystem um.
--
-------------------------------------------------------------------

WITH Ada.Text_Io, Ada.Integer_Text_Io,Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_Io,Ada.Strings, Ada.Characters.Handling;
USE  Ada.Text_Io, Ada.Integer_Text_Io,Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_Io,Ada.Strings;

PROCEDURE Stellenwertsysteme IS
   
   Positiv : Boolean := True;
   Zahlen : String :="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   ZahlenWerte : ARRAY(0 .. 35) OF Integer;

   Basisdereingabe : Integer;
   Basisderausgabe : Integer;
   Eingabezahl : Unbounded_String := Null_Unbounded_String;

   Temp : Integer := 0;
   Temp2 : Integer := 0;
   
   Weiter : Integer := 0;
   Weiter2 : Integer := 0;
   
   Abbruch : Boolean := False;
   Checking : Boolean := True;
   
   Ergebnis : Unbounded_String := Null_Unbounded_String;

BEGIN

   FOR Count IN 0..35 LOOP         -- ZahlenWerte ARRAY wird mit Werten gef�llt.
      ZahlenWerte(Count) := Count;
   END LOOP;      

   Ada.Text_IO.Put ("Basis der Eingabe: ");         -- Benutzer gibt eine Dezimalzahl im Bereich 2 - 36 ein.
   Get (Basisdereingabe);

   Skip_Line;
   
   Ada.Text_IO.Put ("Eingabezahl: ");         -- Benutzer gibt eine Zahl ein,
   Get_Line(Eingabezahl);                     -- diese wird als Unbounded_String gelesen.

   IF Element (Eingabezahl, 1) = '-' THEN         -- Falls die eingegebene Zahl negativ ist
      Positiv := False;                           -- wird dies vermerkt und das Minus Zeichen gel�scht.
      Delete (Eingabezahl, 1, 1);
   END IF;
   
   FOR Check IN 1..Length(Eingabezahl) LOOP         -- Wenn die Eingabezahl andere Zeichen als '0'-'9', 'a'-'z' oder 'A'-'Z' enth�lt wird abgebrochen.
      Checking := Ada.Characters.Handling.Is_Alphanumeric(Element(Eingabezahl, Check));
      IF Checking = False THEN
         Abbruch := True;
      END IF;
   END LOOP;
      
   FOR Laufvar IN reverse 1..Length(Eingabezahl) LOOP
      
      FOR Index IN 1..36 LOOP         -- F�r jedes als Zahl eingegebene Zeichen wird der dazugeh�rige Wert ermittelt.
        IF Zahlen(Index) = Ada.Characters.Handling.To_Upper(Element(Eingabezahl, Laufvar)) THEN
            Temp := ZahlenWerte(Index - 1);
        END IF;
        
        IF Basisdereingabe < Temp THEN         -- Falls die Basis der Eingabe kleiner ist als die Eingabezahl wird abgebrochen.
            Abbruch := True;
            EXIT;
        END IF;
            
      END LOOP;
      IF Abbruch = True THEN         -- Abbruch wird intialisiert.
         EXIT;
      END IF;    

      Temp2 := Temp2 + (Temp * (Basisdereingabe ** (Length(Eingabezahl) - Laufvar) ));         -- Umrechnung in das Dezimalsystem (Basis 10).
   END LOOP;
   

   IF Abbruch = True THEN
      Ada.Text_IO.Put ("Eingabe nicht verstanden.");         -- Abbruch wird ausgef�hrt.
      DELAY 1.0;
   ELSE
      Ada.Text_IO.Put ("Basis der Ausgabe: ");         -- Benutzer gibt eine Dezimalzahl im Bereich 2 - 36 ein.
      Get (Basisderausgabe);

      Weiter := Temp2;
   
      WHILE Weiter /= 0 LOOP
                  
         Temp2 :=  Weiter / Basisderausgabe;         -- Wert wird ganzzahlig geteilt.      
         Weiter2 := Weiter mod Basisderausgabe;         -- Rest wird ermittelt.      
         
         IF Weiter2 > 9 THEN         -- Falls der Wert gr��er ist als 9 wird der Buchstabe ermittelt und an das bisherige Ergebnis angeh�ngt.
            Ergebnis := " " & Zahlen(Weiter2 + 1) & Ergebnis;
         ELSE
            Ergebnis := Integer'Image(Weiter2) & Ergebnis;
         END IF;
      
         Weiter := Temp2;
      END LOOP;
   
      IF Positiv = False THEN         -- Hier wird ein Minus an das Ergebnis angeh�ngt falls die Eingabezahl negativ war.
         Ergebnis := '-' & Ergebnis;
      END IF;

      Ada.Text_IO.Put ("Ausgabe: ");
      Put_Line(Ergebnis);
   end if;
   
END Stellenwertsysteme;
