--  FILE:    Liste.ads
--  PROJECT: Programmieruebungen, Uebungsblatt 8
--  VERSION: 1.0
--  DATE:    06.01.2007
--  AUTHOR: http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 8.1: Einfach verkettete Listen
--
--  Das Package bietet Funktionen und Prozeduren rund um Einfach
--  verkettete Listen wie das einfache Ausgeben der Liste,
--  die Liste Umkehren und Sortieren.
--
-------------------------------------------------------------------
package Liste is

   type Zelle;
   
   type Zeiger is access Zelle;
   
   --  PROCEDURE Load
   --
   --  Es werden Integer aus der Datei die als Parameter angegeben
   --  wurde aus der Datei gelesen und in einem neuen Element
   --  gespeichert welches an die Liste angehängt wird.
   --
   --  PARAMETERS:
   --  + Name - Name der Datei aus der gelesen werden soll.
   --  + Z - Listenanker
   procedure Load (Name : in String; Z : in out Zeiger);
   
   --  PROCEDURE Put
   --
   --  Die Prozedur gibt für jedes Element der Liste
   --  den Inhalt aus.
   --
   --  PARAMETERS:
   --  Z - Listenanker
   procedure Put (Z : in Zeiger);
   
   --  PROCEDURE Freigeben
   --
   --  Gibt den Speicher der Liste frei. 'Z' ist danach eine leere
   --  Liste.
   --
   --  PARAMETERS:
   --  Z - Listenanker
   procedure Freigeben (Z : in out Zeiger);
   
   --  FUNCTION Umkehren
   --
   --  Die funktion Umkehren verwendet die rekursive
   --  Funktion Reku um alle Elemente der gegebenen
   --  Liste umzukehren.
   --
   --  PARAMETERS:
   --  Z - Listenanker
   --  RETURNS:
   --  Die Funktion liefert einen Zeiger auf
   --  die umgekehre Liste zurück.   
   function Umkehren (Z : in Zeiger) return Zeiger;
   
   --  PROCEDURE Selection_Sort
   --
   --  Die Prozedur sortiert die Liste von der
   --  groessten Zahl bis zu kleinsten.
   --  Zum Sortieren der Quell-Liste Z wird
   --  eine anfangs leere Zielliste NewZeiger
   --  verwendet. Der Algorithmus führt solange
   --  Schritte durch, bis Z die leere Liste
   --  geworden ist. In jedem Schritt wird aus Z
   --  das jeweils groesste Element entfernt und
   --  an den Anfang der Liste NewZeiger hinzugefügt.
   --
   --  PARAMETERS:
   --  Z - Listenanker   
   procedure Selection_Sort (Z : in out Zeiger);

   type Zelle is record
      Item            : Integer;
      Next            : Zeiger;
   end record;

end Liste;