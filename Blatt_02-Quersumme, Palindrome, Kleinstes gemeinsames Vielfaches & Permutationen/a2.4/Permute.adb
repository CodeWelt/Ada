--  FILE:    Permute.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 2
--  VERSION: 1.0
--  DATE:    10.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 2.4: Permutationen
--
--  Menschen haben die Fähigkeit in längeren Texten Wörter lesen
--  zu können, an denen nur die Anzahl der Buchstaben sowie
--  der erste und letzte Buchstabe korrekt geschrieben sind.
--  o Das Programm gibt den Text "Gib ein Wort ein: " aus.
--    Der Benutzer gibt ein Wort ein.
--  o Das Programm listet alle möglichen Permutationen des
--    Wortes auf, wobei das erste und das letzte Zeichen des
--    Wortes jeweils undverändert bleiben.
--
-------------------------------------------------------------------

WITH Ada.Text_Io, Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_Io,Ada.Strings;
USE  Ada.Text_Io, Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_Io,Ada.Strings;

PROCEDURE Permute IS

   --  FUNCTION Fakultaet
   --  Die rekursive Funktion berechnet die Fakultät einer gegebenen
   --  Zahl und gibt das Ergebnis zurück.
   --
   --  PARAMETERS:
   --  Param ist die Zahl dessen Fakultät berechnet werden soll.
   --
   --  RETURNS: Die Fakultät der als Parameter gelieferten
   --  Zahl wird als Integer zurückgegeben.
   FUNCTION Fakultaet (Param : Integer) RETURN Integer IS
   BEGIN
      IF Param = 0 THEN
         RETURN 1;
      ELSE
         RETURN Param * Fakultaet(Param - 1);
      END IF;
   END Fakultaet;


   TYPE Permutation IS ARRAY (Integer RANGE <>) OF Integer;
   -- Die Obergrenze ist hier auf 100_000 Buchstaben festgelegt.
   -- Bitte beachten Sie bei der Eingabe, dass bereits bei einem Wort mit
   -- acht Zeichen 720 Permutationen ausgegeben werden.
   Permuteme : Permutation (1..100000);  

   Eingabe : Unbounded_String := Null_Unbounded_String;
   EingabeUser : Unbounded_String := Null_Unbounded_String;
   
   GroesstmoeglicheI : Integer := 0;
   KleinsteElementZ : Integer := Integer'Last;

   Vertauschen1 : Integer := 0;
   Vertauschen2 : Integer := 0;
   BubbleSortVertauschen1 : Integer := 0;
   BubbleSortVertauschen2 : Integer := 0;

   Alphanumeric : String :="0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
   AlphanumericValue : ARRAY(1 .. 62) OF Integer;

   -- Der Counter von der While Schleife wird mit Startwert 1 festgelegt, da
   -- die erste sortierte Permutation vorab erstellt und ausgegeben wird.
   Counter : Integer := 1;
BEGIN
      
   FOR Count IN 1..100000 LOOP   -- Die Werte im zu permutierenden Array werden
      Permuteme(Count) := 0;     -- für den Anfang alle auf Null gesetzt.
   END LOOP;
   
   -- In diesem Schritt wird der Array mit den Werten für jedes Zeichen
   -- im String Alphanumeric gefüllt.
   FOR Countthis IN 1..62 LOOP
      AlphanumericValue(Countthis) := Countthis;
   END LOOP;

   Ada.Text_IO.Put ("Gib ein Wort ein: ");   -- Benutzer gibt ein Wort ein.
   Get_Line(EingabeUser);
   
   -- Das erste und das letzte Zeichen werden gelöscht, bleiben aber in der
   -- Variable EingabeUser zur späteren Konkatenation erhalten.
   Eingabe := EingabeUser;
   Delete (Eingabe, Length(Eingabe), Length(Eingabe));
   Delete (Eingabe, 1, 1);  
   
   -- Hier wird der zu permutierende Array Permuteme mit den
   -- Werten jedes Buchstabens der Eingabe gefüllt.
   FOR Laufvar IN 1..Length(Eingabe) LOOP
      FOR Laufvar2 IN 1..62 loop
         IF Alphanumeric(Laufvar2) = Element(Eingabe, Laufvar) THEN
            Permuteme(Laufvar) := AlphanumericValue(Laufvar2);
         end if;
      end loop;
   end loop;

   -- Dieser einfache Bubble Sort Algorithmus sortiert die Werte
   -- in dem zu permutierenden Array vor der Schleife für den Anfang,
   -- damit am Ende alle möglichen Permutationen ausgegeben werden.
   FOR Unsorted IN REVERSE 1 .. Length(Eingabe) - 1 LOOP
      FOR J IN 1 .. Unsorted LOOP
         IF Permuteme (J) > Permuteme (J+1) THEN
            -- Sortieren durch direktes Austauschen
            BubbleSortVertauschen1 := Permuteme (J);
            BubbleSortVertauschen2 := Permuteme (J + 1);
            Permuteme (J) := BubbleSortVertauschen2;
            Permuteme (J + 1) := BubbleSortVertauschen1;                   
         END IF;
      END LOOP;
   END LOOP;
   
   -- Diese, gerade sortierte, erste Permutation wird als Buchstaben ausgegeben.
   Put(Element(EingabeUser, 1));
   FOR Laufvar IN 1..Length(Eingabe) LOOP
      Put(Alphanumeric(Permuteme(Laufvar)));
   END LOOP;
   Put(Element(EingabeUser, Length(Eingabe) + 2));
   New_Line;
   
   -------------------------------------------------------------------
   -- Hier beginnt der Permutations Algorithmus der solange permutiert
   -- bis die letzte Permutation erreicht wurde.
   -- Gezählt wird vom Startwert 1 bis zur Fakultät von der Anzahl
   -- der zu permutierenden Zeichen.
   -- Der folgende Quelltext wird am Beispiel von Folie 228 erläutert.
   WHILE Counter /= Fakultaet(Length(Eingabe)) LOOP
      
      -- Es wird das größte i mit 
      -- Permuteme(i) < Permuteme(i + 1) ermittelt.
      FOR Index IN 1..Length(Eingabe) LOOP
         IF Index < Length(Eingabe) THEN
            IF Permuteme(Index) < Permuteme(Index + 1) THEN
               GroesstmoeglicheI := Index;
            END IF;
         END IF;
      END LOOP;

      -- Es wird in Permuteme(i + 1) bis Permuteme(n) das kleinste
      -- Element z mit z > Permuteme(i) ermittelt.
      FOR Indexx IN GroesstmoeglicheI + 1 .. Length(Eingabe) LOOP
         IF Permuteme(Indexx) > Permuteme(GroesstmoeglicheI) THEN
            KleinsteElementZ := Indexx;
         END IF;
      END LOOP;

      -- Die gefundenen Zahlen werden ausgetauscht.
      Vertauschen1 := Permuteme(GroesstmoeglicheI);
      Vertauschen2 := Permuteme(KleinsteElementZ);
      Permuteme(GroesstmoeglicheI) := Vertauschen2;
      Permuteme(KleinsteElementZ) := Vertauschen1;

      -- Dieser einfache Bubble Sort Algorithmus sortiert von
      -- Permuteme(i + 1) bis Permuteme(n)
      FOR Unsorted IN REVERSE GroesstmoeglicheI + 1 .. Length(Eingabe) - 1 LOOP
         FOR J IN GroesstmoeglicheI + 1 .. Unsorted LOOP
            IF Permuteme (J) > Permuteme (J+1) THEN
               -- Sortieren durch direktes Austauschen
               BubbleSortVertauschen1 := Permuteme (J);
               BubbleSortVertauschen2 := Permuteme (J + 1);
               Permuteme (J) := BubbleSortVertauschen2;
               Permuteme (J + 1) := BubbleSortVertauschen1;                   
            END IF;
         END LOOP;
      END LOOP;
      
      -- Die ermittelte Permutation wird in Form von Buchstaben ausgegeben.
      Put(Element(EingabeUser, 1));
      FOR Laufvar IN 1..Length(Eingabe) LOOP
         Put(Alphanumeric(Permuteme(Laufvar)));  
      END LOOP;
      Put(Element(EingabeUser, Length(Eingabe) + 2)); 
      New_Line;   

      Counter := Counter + 1;
   END LOOP;
   -------------------------------------------------------------------
END Permute;
