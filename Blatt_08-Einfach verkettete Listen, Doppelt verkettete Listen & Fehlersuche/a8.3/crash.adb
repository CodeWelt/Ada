--  FILE:    Integer_Liste.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 8
--  VERSION: 1.0
--  DATE:    06.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 8.3: Fehlersuche
--
--  Korrigiertes Demonstrationsprogramm fuer einen
--  Programmierfehler.
--
--  Dokumentation der Fehlerursache:
--  Beim ersten Kompilieren des Programms crash.adb tritt folgender
--  fehler auf:
--  raised PROGRAM_ERROR : person_lists.ads:255
--  finalize/adjust raised exception
--  Der Fehler liegt daran dass der Author des Quelltextes zweimal
--  Person_Lists.Destroy aufruft um den Speicher der nicht echten
--  Kopie "Copy" freizugeben. 
--  Der Author hat lediglich den Listenanker Kopiert, nicht aber
--  die gesamte Liste.
--  So kommt es auch zu der beim ersten Kompilieren falschen
--  Ausgabe der "nochmal Originalliste" am ende des ausgeführten
--  Programms.
--  Meine Lösung ist die Funktion Copyx, die von einem gegebenen
--  Listenanker eine neue Kopie der Liste mit allen Elementen
--  erzeugt und den neuen Listenanker zurückgibt. Mit der neuen
--  Liste werden dann die folgenden Schritte durchgeführt und alle
--  Personen, deren Name nicht mit 'H' beginnt werden aus der
--  (echt) kopierten Liste entfernt.
--
-------------------------------------------------------------------
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Person_Lists;

procedure Crash
is

   --  FUNCTION Copyx
   --
   --  Die Funktion Copyx erzeugt eine Kopie
   --  der gegebenen Liste und gibt den Listenanker
   --  der neuen Liste zurück.
   --
   --  PARAMETERS:
   --  Anchor - Der Listenanker dessen Inhalt
   --           kopiert werden soll.
   --  RETURNS:
   --  Die Funktion gibt den Listenanker der neuen
   --  kopierten Liste zurück.
   function Copyx
      (Anchor : in Person_Lists.Cell_Ref)
      return Person_Lists.Cell_Ref
   is
      Cursor  : Person_Lists.List_Cursor := Person_Lists.First (Anchor);
      AnchorCopy : Person_Lists.Cell_Ref;
      Counter : Integer := 0;
      Element : Person_Lists.Person;

   begin
      Person_Lists.Create (AnchorCopy);
      Counter := Person_Lists.Size (Anchor) + 1;
      while Counter /= 0 loop
         Counter := Counter - 1;
         Element := Person_Lists.Get_Element (Cursor);
         Person_Lists.Insert (AnchorCopy, Element);
         Person_Lists.Forward (Cursor);
      end loop;
      
      return AnchorCopy;
   end Copyx; 


   --  PROCEDURE Put_List
   --
   --  Gibt den Inhalt einer Personenliste aus.
   procedure Put_List
     (A : in Person_Lists.Cell_Ref)
   is
      Count   : Natural := 0;
      Cursor  : Person_Lists.List_Cursor := Person_Lists.First (A);
      Element : Person_Lists.Person;
   begin
      while Person_Lists.Is_Valid (Cursor) loop
         Element := Person_Lists.Get_Element (Cursor);
         Count := Count + 1;
         Ada.Integer_Text_IO.Put (Count, 3);
         Ada.Text_IO.Put_Line
           (". " & Ada.Strings.Unbounded.To_String (Element.Name) &
            ": " & Ada.Strings.Unbounded.To_String (Element.Phone));
         Person_Lists.Forward (Cursor);
      end loop;
   end Put_List;


   --  FUNCTION Create_Person
   --
   --  Hilfsfunktion, setzt aus Name und Telefonnummer einen
   --  Person-Record zusammen.
   --
   --  RETURNS: Person-Record, dessen Komponenten durch die Werte der
   --  gleichnamigen Parameter initialisiert wurden.
   function Create_Person
     (Name  : in String;
      Phone : in String)
     return Person_Lists.Person
   is
      use Ada.Strings.Unbounded;
   begin
      return (To_Unbounded_String (Name), To_Unbounded_String (Phone));
   end Create_Person;


   List_Anchor  : Person_Lists.Cell_Ref;
   Copy         : Person_Lists.Cell_Ref;
   Cursor       : Person_Lists.List_Cursor;
   
begin
   --  Erzeugen einer neuen leeren Liste
   Person_Lists.Create (List_Anchor);

   --  Einfuegen einiger Elemente in die Liste
   Person_Lists.Insert (List_Anchor, Create_Person ("Heinz", "+497111234675"));
   Person_Lists.Insert (List_Anchor, Create_Person ("A.", "+497111234567"));
   Person_Lists.Insert (List_Anchor, Create_Person ("Hans", "+497111234657"));
   Person_Lists.Insert (List_Anchor, Create_Person ("Max", "+497111234576"));
   Person_Lists.Insert (List_Anchor, Create_Person ("X.", "+497111234765"));
   Person_Lists.Insert (List_Anchor, Create_Person ("Jack", "+497111234756"));
   Person_Lists.Insert (List_Anchor, Create_Person ("Y.", "+497111235467"));
   
   
   --  Ausgabe der Liste
   Ada.Text_IO.Put_Line ("::: Originalliste:");
   Put_List (List_Anchor);

   --  Erzeugen einer Kopie der Liste (Korrigiert)
   Copy := Copyx (List_Anchor);
   

   --  Bearbeiten der Liste
   Ada.Text_IO.Put_Line ("::: Arbeitsschritte:");
   --  Setzen des Cursors auf das letzte Element in der Liste
   Cursor := Person_Lists.Last (List_Anchor);
   --  Solange noch Elemente vorhanden sind, durchlaufen der Liste in
   --  umgekehrter Sortierreihenfolge.
   while Person_Lists.Is_Valid (Cursor) loop
      declare
         --  Extrahieren des Namens der aktuellen Person
         Name : constant String := Ada.Strings.Unbounded.To_String
           (Person_Lists.Get_Element (Cursor).Name);
      begin
         if Name = "" then
            null;
         elsif Name (Name'First) /= 'H' then
            --  Alle Personen, deren Name nicht mit 'H' beginnt werden
            --  aus der kopierten Liste entfernt.
            Person_Lists.Remove
              (Anchor => Copy,
               Name   => Name);

            --  Ausgabe des Name
            Ada.Text_IO.Put_Line (Name & " aus kopierter Liste entfernt.");
         end if;
      end;

      --  Zurueckbewegen des Cursors auf das vorige Element
      Person_Lists.Backward (Cursor);
   end loop;

   --  Ausgabe des Resultats
   Ada.Text_IO.Put_Line ("::: verkuerzte Kopie:");
   Put_List (Copy);

   Ada.Text_IO.Put_Line ("::: nochmal Originalliste:");
   Put_List (List_Anchor);

   --  Loeschen der Listen aus dem Haldenspeicher
   Person_Lists.Destroy (List_Anchor);
   Person_Lists.Destroy (Copy);
end Crash;
