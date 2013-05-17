--  FILE:    person_lists.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 8
--  VERSION: 1.0
--  DATE:    06.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 8.2: Doppelt verkettete Listen
--
--  Das Package bietet Funktionalität für eine Telefonliste.
--  Die Liste ist zu jedem Zeitpunkt nach Name alphabetisch
--  sortiert.
--
-------------------------------------------------------------------
with Ada.Unchecked_Deallocation, Ada.Text_IO, Ada.Strings.Unbounded;
use Ada.Text_IO, Ada.Strings.Unbounded;

package body Person_Lists is

   --  GENERIC INSTANCE Free
   --
   --  Instanziert Ada.Unchecked_Deallocation, um mit "new" allokierte
   --  Cell-Objekte freizugeben. Die Instanz ist ein Unterprogramm
   --  mit folgender Deklaration:
   --
   --  --  PROCEDURE Free
   --  --
   --  --  Gibt den Speicher frei, der mit 'new Cell' allokiert
   --  --  wurde. Tut nichts, falls 'X = null'. Setzt X auf
   --  --  null. Darf auf keinen Fall mehrmals fuer das selbe Objekt
   --  --  aufgerufen werden.
   --  --  Siehe ARM 13.11.2(6-10/2, 16)
   --  --
   --  --  PARAMETERS:
   --  --  + X - Zeiger auf das freizugebende Objekt, wird auf 'null'
   --  --        gesetzt.
   --  --  POST: X = null
   --  procedure Free
   --    (X : in out Cell_Ref);
   --
   procedure Free is new Ada.Unchecked_Deallocation
     (Name   => Cell_Ref,
      Object => Cell);

   --  PROCEDURE Create
   --
   --  Erzeugt eine leere Liste und setzt den Listenanker 'Anchor'
   --  entsprechend.
   procedure Create
     (Anchor : out Cell_Ref)
   is
   begin
      Anchor := new Cell;
   end Create;

   --  FUNCTION Is_Empty
   --
   --  Gibt 'True' zurueck, falls die uebergebene Liste leer ist,
   --  ansonsten 'False'.
   function Is_Empty
     (Anchor : in Cell_Ref)
     return Boolean
   is
   begin
      if Anchor = null then
         return True;
      else
         return False;
      end if;
   end Is_Empty;

   --  FUNCTION Size
   --
   --  Ermittelt die Anzahl der Elemente in der Liste.
   function Size
     (Anchor : in Cell_Ref)
     return Natural
   is
      Counter : Natural := 0;
      Temp_Anchor : Cell_Ref;
      Temp_List_Cursor : List_Cursor;
   begin
      Temp_Anchor := Anchor;
      Temp_List_Cursor := Person_Lists.First (Temp_Anchor);
      --  Die Schleife läuft so lange bis kein zulässiges
      --  Element mehr vorhanden ist.
      while Person_Lists.Is_Valid (Temp_List_Cursor) loop
         --  Der Zähler wird um eins erhöht.
         Counter := Counter + 1;
         Person_Lists.Forward (Temp_List_Cursor);
      end loop;
      return Counter - 1;
   end Size;

   --  FUNCTION Contains
   --
   --  Ermittelt, ob in der Liste 'Anchor' ein Eintrag existiert,
   --  dessen 'Name'-Komponente den gleichen Wert hat, wie der
   --  aktuelle Parameter 'Name'.
   --
   --  PARAMETERS:
   --  + Anchor - Listenanker
   --  + Name - Name einer gesuchten Person
   function Contains
     (Anchor : in Cell_Ref;
      Name   : in String)
     return Boolean
   is
      LCursor : List_Cursor;
   begin
      --  Find_Element wird dazu benützt um ein Element mit
      --  dem selben namen zu finden.
      LCursor := Find_Element (Anchor, Name);
      --  Falls eins vorhanden war wird True zurückgegeben,
      --  andernfalls False.
      if LCursor = null then
         return False;
      else
         return True;
      end if;
      
   end Contains;

   --  FUNCTION AGroesserAlsB
   --
   --  Hier wird ermittelt welcher der beiden übergebenen
   --  Namen alphabetisch zuerst geschrieben wird.
   --
   --  PARAMETERS:
   --  + NameA - Erster Name.
   --  + NameB - Zweiter Name.
   function AGroesserAlsB
      (NameA : in Unbounded_String;
       NameB : in Unbounded_String)
      return Boolean
   is
      Zahlen : String := "0123456789AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTt" &
      "UuVvWwXxYyZz";
      ZahlenWerte : array (0 .. 61) of Integer;
      LengthGroesser : Natural := 0;
      WertBuchstabeA, WertBuchstabeB : Natural := 0;
      LengthNameA, LengthNameB : Natural := 0;
   begin
      --  ZahlenWerte ARRAY wird mit Werten gefüllt.
      for Count in ZahlenWerte'Range loop
         ZahlenWerte (Count) := Count;
      end loop;
   
      --  Es wird das kleinere Wort ermittelt um in der
      --  folgenden Schleife bis zu dieser Grenze durchzulaufen.
      LengthNameA := Length (NameA);
      LengthNameB := Length (NameB);
      if LengthNameA > LengthNameB then
         LengthGroesser := LengthNameB;
      else
         LengthGroesser := LengthNameA;
      end if;
      
      --  Die Schleife läuft für jedes Zeichen im kleineren Wort.
      for Laufvar in 1 .. LengthGroesser loop
         
         --  Nur wenn die Buchstaben nicht gleich sind wird fortgefahren.
         if Element (NameA, Laufvar) /= Element (NameB, Laufvar) then
         
            --  Die folgenden 2 Schleifen ermitteln den wert
            --  der Beiden aktuellen Buchstaben um daraufhin zu vergleichen
            --  welcher höherwertig ist.
            for Index in 1 .. 62 loop
               if Zahlen (Index) = Element (NameA, Laufvar) then
                  WertBuchstabeA := ZahlenWerte (Index);
                  exit;
               end if;
            end loop;
            

            for Index in 1 .. 62 loop
               if Zahlen (Index) = Element (NameB, Laufvar) then
                  WertBuchstabeB := ZahlenWerte (Index);
                  exit;
               end if;
            end loop;
            
            --  Entweder der erste Buchstabe ist höherwertig als der zweite,
            --  dann wird True zurückgegeben, andernfalls False.
            if WertBuchstabeA /= 0 and WertBuchstabeB /= 0 and
            WertBuchstabeA > WertBuchstabeB then
               return True;
            else
               return False;
            end if;
      
         end if;
                  
      end loop;

      if LengthNameA < LengthNameB then
         return False;
      else
         return True;
      end if;
   
   end AGroesserAlsB;


   --  FUNCTION Find_Bigger
   --
   --  Hier wird die übergebene Liste durchlaufen bis
   --  ein oder kein Element gefunden wurde welches
   --  alphabetisch groesser ist als der übergebene Name
   --  des neuen Elements.
   --
   --  PARAMETERS:
   --  + Anchor - Listenanker
   --  + Namex - Der Name des neuen Elements welches hinzugefügt
   --            werden soll.
   function Find_Bigger (Anchor : in Cell_Ref; Namex : in Unbounded_String)
   return Cell_Ref is
      Bigger : Cell_Ref := null;
      Lauf : List_Cursor;
   begin
      Lauf := First (Anchor);
      --  Die Schleife durchläuft alle Elemente der Liste.
      while Lauf.Next /= null loop
         --  Wenn der Name des aktuellen Elements groesser ist als
         --  der Name des Elements welches eingefügt werden soll,
         --  wird das aktuelle Element zurückgegeben, um das neue Element davor
         --  einzufügen, andernfalls wird ein Leerer Cell_Ref zurückgegeben.
         if AGroesserAlsB (Lauf.Next.Data.Name, Namex) = True then
            Bigger := Lauf.Next;
            return Bigger;
         end if;
         Forward (Lauf);
      end loop;
      
      return Bigger;

   end Find_Bigger;

   --  PROCEDURE Insert
   --
   --  Fuegt ein neues Element an der richtigen Position in die Liste
   --  ein.  Die Liste bleibt dadurch alphabetisch aufsteigend
   --  sortiert.  Es dürfen mehrere gleiche Elemente in der Liste
   --  gespeichert werden.  Haben zwei Elemente den gleichen Namen, so
   --  ist ihre Reihenfolge beliebig.
   procedure Insert
     (Anchor      : in out Cell_Ref;
      New_Element : in     Person)
   is
      Z2            : Cell_Ref  := null;
      Ref : Cell_Ref;
      CRefPrev, CRefNext : Cell_Ref;
      Empty : Cell_Ref;
   begin
      Empty := new Cell;

      --  Wenn die Liste zuvor noch kein gesetztes Element
      --  besitzt wird das Erste eingefügt.
      if Anchor = Empty then
         Z2 := new Cell'(null, null, New_Element);
         Anchor := Z2;
      else
         --  Es wird mit Hilfe der Funktion Find_Bigger
         --  das erste Element gesucht welches groesser ist
         --  als das welches eingefügt werden soll.
         Ref := Find_Bigger (Anchor, New_Element.Name);
         --  Wenn ein Leerer List_Curser von Find_Bigger
         --  zurückgegeben wurde, so wurde in der gesamten
         --  Liste kein groesseres Element gefunden.
         if Ref = null then
            --  Das neue Element wird an das Ende der Liste
            --  angehängt.
            Z2 := new Cell'(null, Anchor, New_Element);
            Anchor.Next := Z2;
            Anchor := Z2;
         else
            --  Wenn die Funktion Find_Bigger ein Element
            --  gefunden hat, so wird das neue Element genau
            --  vor das groessere platziert.
            if Ref.Prev /= null then
            
               --  Eine neue Zelle wird mit den dazugehörigen
               --  zwei pointern erzeugt und die beiden
               --  Pointer welche die neue Zelle umgeben
               --  ihre neuen Werte zugewiesen.
               CRefPrev := Ref.Prev;
               
               Ref := Ref.Prev;
               CRefNext := Ref.Next;
               
               Z2 := new Cell'(CRefNext, CRefPrev, New_Element);
               Ref.Next := Z2;
               
               Ref := Z2.Next;
               Ref.Prev := Z2;
            end if;
         end if;

         if Contains (Anchor, "") then
            Remove (Anchor, "");
         end if;

      end if;
   end Insert;

   --  PROCEDURE Remove
   --
   --  Loescht Alle Eintraege aus der Liste, die einen bestimmten
   --  Namen besitzen.  Die Komponente 'Element.Phone' wird nicht
   --  beachtet.
   --
   --  PARAMETERS:
   --  + Anchor - Listenanker
   --  + Name - Alle Elemente, deren 'Name' Komponente gleich dem
   --  aktuellen Parameter ist, sollen aus der Liste gelöscht werden.
   --  RAISES:
   --  + Not_In_List - falls kein Element mit dem gesuchten Namen in
   --  der Liste enthalten ist.
   procedure Remove
     (Anchor : in out Cell_Ref;
      Name   : in     String)
   is
      LCursor : List_Cursor;
      CRef, CRef2 : Cell_Ref;
   begin
      --  Nur wenn die Liste den Namen der gelöscht werden
      --  soll enthält, wird fortgefahren.
      if Contains (Anchor, Name) = True then
         --  Es wird das Element gesucht, welches gelöscht
         --  werden soll.
         LCursor := Find_Element (Anchor, Name);
         --  Es werden die vom zu löschenden Objekt
         --  umgebenen Pointer neu zugewiesen.
         --  Somit ist das Element kein Teil der
         --  Liste mehr.
         CRef := LCursor.Prev;
                  
         if CRef = null and LCursor.Data.Name = "" then
            Forward (LCursor);
            LCursor.Prev := null;
         else
            CRef2 := LCursor.Next;
            Backward (LCursor);
            LCursor.Next := CRef2;
            
            if LCursor.Next /= null then
               Forward (LCursor);
               LCursor.Prev := CRef;         
            end if;         
         end if;         

      else
         Put_Line ("Der Name " & Name & " ist nicht vorhanden.");
         raise Not_In_List;
      end if;
   end Remove;

   --  PROCEDURE Destroy
   --
   --  Gibt den Speicher der Liste frei. 'Anchor' ist danach eine leere
   --  Liste.
   --
   --  POST: Is_Empty (Anchor)
   procedure Destroy
     (Anchor : in out Cell_Ref)
   is
      Tmp : Cell_Ref := null;
   begin
      --  Die Schleife durchläuft die Liste.
      loop
         Tmp := Anchor;
         --  Der Speicher des aktuellen
         --  Elements wird freigegeben.
         Free (Tmp);
         Anchor := Anchor.Next;
         exit when Is_Empty (Anchor) = True;
      end loop;
      --  Ada.Text_Io.Put_Line ("Speicher wurde freigegeben.");   
   end Destroy;


   -------------
   -- Cursors --
   -------------


   function Is_Valid
     (Cursor : in List_Cursor)
     return Boolean
   is
   begin
      return Cursor /= null;
   end Is_Valid;

   --  FUNCTION Find_Element
   --
   --  Sucht in der Liste 'Anchor' nach einem Element mit dem Namen
   --  'Name' und gibt einen Cursor zurück, der dieses Element
   --  referenziert.  Existiert kein solches Element, so wird ein
   --  ungueltiger Cursor zurueckgegeben.
   --
   --  RETURNS: Cursor mit Is_Valid (Cursor), falls
   --             Get_Element (Cursor).Name = Name
   --           oder Cursor mit not Is_Valid (Cursor), falls kein
   --             passendes Element von Anchor aus erreichbar ist.
   function Find_Element
     (Anchor : in Cell_Ref;
      Name   : in String)
     return List_Cursor
   is
      Temp_Anchor : Cell_Ref;
      Temp_List_Cursor : List_Cursor;
   begin
      Temp_Anchor := Anchor;
      Temp_List_Cursor := Person_Lists.First (Temp_Anchor);
      --  Die Schleife durchläuft alle Elemente in der Liste.
      while Person_Lists.Is_Valid (Temp_List_Cursor) loop
         --  Wenn der Name mit dem aktuellen Element
         --  übereinstimmt wird dieses Element zurückgegeben.
         if Temp_List_Cursor.Data.Name = Name then
            return Temp_List_Cursor;
         end if;
         Person_Lists.Forward (Temp_List_Cursor);
      end loop;
      --  Falls kein passendes Element erreichbar ist.       
      return null;

   end Find_Element;


   function First
     (Anchor : in Cell_Ref)
     return List_Cursor
   is
      Current  : Cell_Ref := Anchor;
      Previous : Cell_Ref := Anchor;
   begin
      while Previous /= null loop
         Current := Previous;
         Previous := Previous.all.Prev;
      end loop;
      return List_Cursor (Current);
   end First;


   function Last
     (Anchor : in Cell_Ref)
     return List_Cursor
   is
      Current : Cell_Ref := Anchor;
      Next    : Cell_Ref := Anchor;
   begin
      while Next /= null loop
         Current := Next;
         Next := Current.all.Next;
      end loop;
      return List_Cursor (Current);
   end Last;


   function Get_Element
     (Cursor : in List_Cursor)
     return Person
   is
   begin
      if Cursor = null then
         raise Invalid_Cursor;
      else
         return Cursor.all.Data;
      end if;
   end Get_Element;


   procedure Forward
     (Cursor : in out List_Cursor)
   is
   begin
      if Cursor = null then
         raise Invalid_Cursor;
      else
         Cursor := List_Cursor (Cursor.all.Next);
      end if;
   end Forward;


   procedure Backward
     (Cursor : in out List_Cursor)
   is
   begin
      if Cursor = null then
         raise Invalid_Cursor;
      else
         Cursor := List_Cursor (Cursor.all.Prev);
      end if;
   end Backward;


end Person_Lists;
