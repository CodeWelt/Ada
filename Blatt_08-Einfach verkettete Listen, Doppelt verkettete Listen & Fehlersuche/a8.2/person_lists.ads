--  FILE:    person_lists.ads
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
with Ada.Strings.Unbounded;

package Person_Lists is


   --  TYPE Person
   --
   --  Elementtyp der Liste, speichert Name und Telefonnummer einer Person
   type Person is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Phone : Ada.Strings.Unbounded.Unbounded_String;
      end record;


   --  TYPE Cell_Ref
   --
   --  Listenanker
   type Cell_Ref is private;


   --  EXCEPTION Not_In_List
   --
   --  Wird erhoben, immer wenn eine Operation auf einem Element
   --  ausgeführt werden soll, das nicht in der Liste enthalten ist.
   Not_In_List : exception;


   --  PROCEDURE Create
   --
   --  Erzeugt eine leere Liste und setzt den Listenanker 'Anchor'
   --  entsprechend.
   procedure Create
     (Anchor : out Cell_Ref);


   --  FUNCTION Is_Empty
   --
   --  Gibt 'True' zurueck, falls die uebergebene Liste leer ist,
   --  ansonsten 'False'.
   function Is_Empty
     (Anchor : in Cell_Ref)
     return Boolean;


   --  FUNCTION Size
   --
   --  Ermittelt die Anzahl der Elemente in der Liste.
   function Size
     (Anchor : in Cell_Ref)
     return Natural;


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
     return Boolean;


   --  PROCEDURE Insert
   --
   --  Fuegt ein neues Element an der richtigen Position in die Liste
   --  ein.  Die Liste bleibt dadurch alphabetisch aufsteigend
   --  sortiert.  Es dürfen mehrere gleiche Elemente in der Liste
   --  gespeichert werden.  Haben zwei Elemente den gleichen Namen, so
   --  ist ihre Reihenfolge beliebig.
   procedure Insert
     (Anchor      : in out Cell_Ref;
      New_Element : in     Person);


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
      Name   : in     String);


   --  PROCEDURE Destroy
   --
   --  Gibt den Speicher der Liste frei. 'Anchor' ist danach eine leere
   --  Liste.
   --
   --  POST: Is_Empty (Anchor)
   procedure Destroy
     (Anchor : in out Cell_Ref);



   --  TYPE List_Cursor
   --
   --  Bietet die Möglichkeit ein bestimmtes Element der Liste zu
   --  referenzieren.  Ein Cursor besitzt einen Status
   --  (gueltig/ungueltig), modelliert durch die Funktion 'Is_Valid'.
   --  Die Funktion gibt 'True' zurueck, solange ein Cursor ein
   --  Element der Liste referenziert (gueltig), 'False' falls der
   --  Cursor ueber eines der Listenenden hinausbewegt wurde
   --  (ungueltig).
   type List_Cursor is private;


   --  EXCEPTION Invalid_Cursor
   --
   --  Wird erhoben, falls auf das Element eines ungueltigen Cursors
   --  zugegriffen wird.
   Invalid_Cursor : exception;


   --  FUNCTION Is_Valid
   --
   --  Gibt 'True' zurueck, falls 'Cursor' in Status gueltig ist,
   --  d.h. ein Listenelement referenziert.  Gibt 'False' zurueck,
   --  falls der 'Cursor' ungueltig ist, d.h. nicht initialisiert ist
   --  oder ueber ein Listenende hinaus bewegt wurde.
   function Is_Valid
     (Cursor : in List_Cursor)
     return Boolean;


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
     return List_Cursor;


   --  FUNCTION First
   --
   --  Gibt einen Cursor zurueck, der die erste Listenzelle
   --  referenziert.  Ist die Liste leer, so ist der Cursor 'not
   --  Is_Valid'.
   function First
     (Anchor : in Cell_Ref)
     return List_Cursor;


   --  FUNCTION Last
   --
   --  Gibt einen Cursor zurueck, der die letzte Listenzelle
   --  referenziert.  Ist die Liste leer, so ist der Cursor 'not
   --  Is_Valid'.
   function Last
     (Anchor : in Cell_Ref)
     return List_Cursor;


   --  FUNCTION Get_Element
   --
   --  Gibt den Inhalt der durch 'Cursor' referenzierten Zelle
   --  zurueck.  Ist der 'Cursor' nicht gueltig, dann erhebt
   --  'Invalid_Cursor'.
   --
   --  RAISES:
   --  * Invalid_Cursor - falls 'not Is_Valid (Cursor)';
   function Get_Element
     (Cursor : in List_Cursor)
     return Person;


   --  PROCEDURE Forward
   --
   --  Bewegt den Cursor eine Zelle weiter.  Falls der Cursor das
   --  Listenende ueberschreitet, so wird er ungueltig.
   --
   --  RAISES:
   --  * Invalid_Cursor - falls 'not Is_Valid (Cursor)'
   --  PRE: Is_Valid (Cursor)
   procedure Forward
     (Cursor : in out List_Cursor);


   --  PROCEDURE Backward
   --
   --  Bewegt den Cursor eine Zelle zurueck.  Falls der Cursor den
   --  Listenanfang ueberschreitet, so wird er ungueltig.
   --
   --  RAISES:
   --  * Invalid_Cursor - falls 'not Is_Valid (Cursor)'
   --  PRE: Is_Valid (Cursor)
   procedure Backward
     (Cursor : in out List_Cursor);


private

   type Cell;

   --  TYPE Cell_Ref
   --
   --  Zeiger auf ein Listenelement.
   type Cell_Ref is access Cell;

   --  TYPE Cell
   --
   --  Listenzelle
   type Cell is
      record
         --  Zeiger auf die nachfolgende Listenzelle, oder null falls
         --  dies die letzte Zelle ist.
         Next : Cell_Ref;
         --  Zeiger auf die vorhergehende Listenzelle, oder null falls
         --  dies die erste Zelle ist.
         Prev : Cell_Ref;
         --  Listenelement, das in dieser Zelle gespeichert ist.
         Data : Person;
      end record;


   --  TYPE List_Cursor
   --
   --  Umbenennung des Cell_Ref-Zeigers als Cursor-Abstraktion.
   --  Gueltige Cursor zeigen auf eine Listen-Zelle, ungueltige haben
   --  den Wert null.
   type List_Cursor is new Cell_Ref;

end Person_Lists;
