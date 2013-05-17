--  FILE:    Liste.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 8
--  VERSION: 1.0
--  DATE:    06.01.2007
--  AUTHOR:  http://CodeWelt.com
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
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Unchecked_Deallocation;
use Ada.Text_IO;

package body Liste is
 
   File : Ada.Text_IO.File_Type;

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
   procedure Free is new Ada.Unchecked_Deallocation (Zelle, Zeiger);

   --  PROCEDURE Load
   --
   --  Es werden Integer aus der Datei die als Parameter angegeben
   --  wurde aus der Datei gelesen und in einem neuen Element
   --  gespeichert welches an die Liste angehängt wird.
   --
   --  PARAMETERS:
   --  + Name - Name der Datei aus der gelesen werden soll.
   --  + Z - Listenanker
   procedure Load (Name : in String; Z : in out Zeiger) is
      T : Zeiger := Z;
      Zahl : Integer := 0;
   begin
      Freigeben (Z);
      --  Die Datei, welche als Parameter übergeben
      --  wurde, wird geöffnet.
      Ada.Text_IO.Open (File, In_File, Name);
      --  Die Schleife läuft solange bis das Ende
      --  der Datei erreicht wird.
      while not Ada.Text_IO.End_Of_File (File) loop
         --  Es wird ein Integer aus der Datei gelesen
         --  und in der Variable Zahl gespeichert.
         Ada.Integer_Text_IO.Get (File, Zahl);
         --  Falls in der Liste noch kein Element vorhanden war,
         --  wird das erste erzeugt.
         if T = null then
            T := new Zelle;
            T.Item := Zahl;
            Z := T;
         --  Falls die Liste nicht leer war, wird
         --  ein Element angehängt.
         else
            T.Next := new Zelle;
            T := T.Next;
            T.Item := Zahl;
         end if; 
      end loop;
      Ada.Text_IO.Close (File);
      
      exception
         when Name_Error => Ada.Text_IO.Put_Line ("Datei nicht gefunden");
         when Data_Error => Ada.Text_IO.Put ("Unix,Dos? Oder auch nicht!");
   end Load;

   --  PROCEDURE Put
   --
   --  Die Prozedur gibt für jedes Element der Liste
   --  den Inhalt aus.
   --
   --  PARAMETERS:
   --  Z - Listenanker
   procedure Put (Z : in Zeiger) is
      Tmp : Zeiger := Z;
   begin
      --  Die Schleife läuft solange bis das Ende
      --  der Liste erreicht wurde.
      while Tmp /= null loop
         --  Jeder Inhalt des aktuellen Elements
         --  wird ausgegeben.
         Ada.Text_IO.Put_Line (Tmp.Item'Img);
         Tmp := Tmp.Next;
      end loop;
   end Put;
   
   --  PROCEDURE Freigeben
   --
   --  Gibt den Speicher der Liste frei. 'Z' ist danach eine leere
   --  Liste.
   --
   --  PARAMETERS:
   --  Z - Listenanker
   procedure Freigeben (Z : in out Zeiger) is
      Tmp : Zeiger := null;
   begin
      Tmp := Z;
      --  Die Schleife läuft für jedes Element
      --  der Liste.
      loop
         Free (Tmp);
         --  Die Schleife wird dann beendet wenn
         --  Tmp zur leeren Liste geworden ist.
         exit when Tmp = null;
         Tmp := Tmp.Next;
      end loop;
            
      --  Ada.Text_Io.Put_Line ("Speicher wurde freigegeben.");
   end Freigeben;

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
   function Umkehren (Z : in Zeiger) return Zeiger is

      --  PROCEDURE Reku
      --
      --  Die rekursive Funktion Reku füllt die
      --  neue umgekehre Liste T immer mit dem
      --  aktuellen Element.
      --
      --  PARAMETERS:
      --  Z - Listenanker
      --  T - Listenanker   
      procedure Reku (Z : in Zeiger; T : in out Zeiger) is
      begin
         --  Abbruchbedingung, wenn das Ende der
         --  Liste erreicht wurde.
         if Z /= null then
            T := new Zelle'(Z.Item, T);
            Reku (Z.Next, T);
         end if;
      end Reku;
      
      T : Zeiger := null;
      Temp : Zeiger := null;
   begin
      Temp := Z;     
      Reku (Z, T);
      --  Freigeben der unsortierten Liste.
      Freigeben (Temp);
      return T; 
   end Umkehren;


   --  FUNCTION Find_Max
   --
   --  Die Funktion ermittelt das Element mit dem
   --  groessten Inhalt und gibt einen Zeiger darauf
   --  zurück.
   --
   --  PARAMETERS:
   --  Z - Listenanker  
   --  RETURNS:
   --  Die Funktion liefert einen Zeiger zurück
   --  der auf das groesste Element der liste zeigt.
   function Find_Max (Z : in Zeiger) return Zeiger is
      Lauf, Max : Zeiger := Z;
   begin
      --  Die Schleife läuft solange bis das Ende der
      --  Liste erreicht wird.
      while Lauf.Next /= null loop
         if Max.Item < Lauf.Next.Item then
            Max := Lauf.Next;
         end if;
         Lauf := Lauf.Next;
      end loop;
      return Max;
      
   end Find_Max;

   --  PROCEDURE Deletex
   --
   --  Die Prozedur löscht ein Element aus
   --  der gegebenen Liste.
   --
   --  PARAMETERS:
   --  Z - Listenanker
   --  E - Zeiger auf das Element welches gelöscht
   --      werden soll.
   procedure Deletex (E : Zeiger; A : in out Zeiger) is
      p, q : Zeiger;
      HatWas : Boolean := False;
   begin
      p := A;
      q := null;
      
      --  Die Schleife läuft solange bis der aktuelle Inhalt
      --  nicht gleich dem Inhalt des gegebenen Elements ist.
      while p.Item /= E.Item and then p /= null loop
         q := p;
         p := p.Next;
         HatWas := True;
      end loop;
 
      if p /= null then
         if q = null then
            if A.Item = E.Item then
               A := A.Next;
            end if;
         else
            --  Das Element wird durch Auslinken
            --  aus der Liste genommen.
            q.Next := p.Next;
         end if;
      end if;

   end Deletex;

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
   procedure Selection_Sort (Z : in out Zeiger) is
      T : Zeiger := Z;
      NewListZeiger : Zeiger;
      NewZeiger : Zeiger;
      Counter : Natural := 0;
   begin
      --  Solange Z nicht zur leeren Liste
      --  geworden ist, wird fortgefahren.
      while Z /= null loop
         Counter := Counter + 1;
         --  Die Funktion Find_Max ermittelt
         --  das groesste Element in der Liste
         --  Z und gibt einen Zeiger auf dieses
         --  zurück.
         T := Find_Max (Z);
         
         --  Das groesste Element, das gerade ermittelt
         --  wurde, wird an den Anfang der neuen
         --  sortierten Liste gehängt.
         if NewListZeiger = null then
            NewListZeiger := new Zelle;
            NewListZeiger.Item := T.Item;
            NewZeiger := NewListZeiger;
         else
            NewListZeiger.Next := new Zelle;
            NewListZeiger := NewListZeiger.Next;
            NewListZeiger.Item := T.Item;
         end if;
         
         --  Aus der unsortierten Liste
         --  wird dieses Max Element entfernt.
         Deletex (T, Z);
      end loop;
      
      Z := NewZeiger;
   end Selection_Sort;

end Liste;