--  FILE:    Median.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 10
--  VERSION: 1.0
--  DATE:    21.01.2007
--  AUTHOR: http://CodeWelt.com
--
-------------------------------------------------------------------
-- 
--  Aufgabe 10.1: Median-Berechnung
--
--  Falls die Anzahl der Zahlen in der Folge ungerade ist, so ist
--  der Obermedian dieser Folge definiert als das mittlere
--  Element der Folge. Ist die Anzahl der Zahlen in der Folge
--  gerade, so ist der Obermedian das rechte Element des in der
--  Mitte stehenden Paars.
--  Durch wiederhoten Aufruf der Prozedur Add_Number werden
--  ganze Zahlen an die anfangs leere Folge angefügt.
--  Durch einen Aufruf der Funktion Result wird der Obermedian
--  berechnet und zurück gegeben. Das Paket kann beliebig lange
--  Folgen von Zahlen verarbeiten.
--
-------------------------------------------------------------------
with Ada.Text_IO, Ada.Unchecked_Deallocation;
use  Ada.Text_IO;

package body Median is

   Anchor : Zeiger := null;
   ElementCounter : Integer := 0;

   procedure Free is new Ada.Unchecked_Deallocation (Zelle, Zeiger);

   --  PROCEDURE Reset
   --
   --  Es werden die bereits eingefügten Zahlen vergessen und
   --  erneut mit der leeren Folge gestartet.
   procedure Reset is
      Tmp : Zeiger := null;
   begin
      --  Wenn der Listen-Anker bereits leer ist
      --  wird nichts unternommen.
      if Anchor /= null then
         --  Die Schleife läuft solange der Listen
         --  Anker nicht leer ist.
         loop
            Tmp := Anchor;
            --  Es wird ein Element weiter in der
            --  Liste gesprungen.
            Anchor := Anchor.Next;
            --  Das übersprungene Element der Liste
            --  wird mit Free Freigegeben.
            Free (Tmp);
            exit when Anchor = null;
         end loop; 
         ElementCounter := 0;
      else
         Put_Line ("Die Liste ist bereits Leer.");
      end if;
   end Reset;
   
   --  FUNCTION Sort_In
   --
   --  Es wird die übergebene Zahl sortiert in die Liste
   --  Anchor eingefügt.
   --
   --  PARAMETERS:
   --  + L - Zeiger auf die Liste in die Eingefügt
   --        werden soll.
   --  + I - Die Zahl als Integer welche sortiert
   --        eingefügt werden soll.
   --  RETURNS:
   --  Die Funktion liefert den Zeiger auf die Liste in
   --  die eingefügt wurde zurück.
   function Sort_In (L : in Zeiger; I : in Integer) return Zeiger is
      Tmp : Zeiger := L;
      H     :  Zeiger;
   begin
      --  Wenn die Liste leer ist wird eine neue
      --  Zelle erzeugt.
      if L = null or else L.Inhalt >= I then
         Tmp := new Zelle'(I, L);
         return Tmp;
      end if;
      --  Mit hilfe der while Schleife wird das Element
      --  der Liste gesucht welches groesser als die
      --  übergebene Zahl ist.
      while Tmp.Next /= null and then Tmp.Next.Inhalt < I loop
         Tmp := Tmp.Next;
      end loop;
      --  Es wird eine neue Zelle erzeugt die genau
      --  an der stelle eingefügt wird, wo die While
      --  Schleife aufgehört hat.
      H := new Zelle'(I, Tmp.Next);
      Tmp.Next := H;
      return L;
   end Sort_In;

   --  PROCEDURE Add_Number
   --
   --  Die übergebene Zahl wird mit Hilfe
   --  von der Funktion Sort_In sortiert
   --  in die Liste Anchor eingefügt.
   --  PARAMETERS:
   --  Number : Die Zahl als Integer Wert die
   --  sortiert eingefügt werden soll.
   procedure Add_Number
     (Number : in Integer)
   is
   begin
      Anchor := Sort_In (Anchor, Number);
      ElementCounter := ElementCounter + 1;
   end Add_Number;

   --  FUNCTION Result
   --
   --  Es wird der Obermedian berechnet und zurück gegeben.
   --
   --  RETURNS:
   --  Die Funktion liefert die Zahl als Integer Wert zurück,
   --  die den Obermedian der Liste bezeichnet.
   function Result
     return Integer
   is
      --  Die Anzahl der sprünge um den Obermedian zu erreichen
      --  wird zunächst durch die Formel (n + 1) / 2 bestimmt.
      JumpCount : Integer := (ElementCounter + 1) / 2;
      TmpAnchor : Zeiger := Anchor;
   begin
      --  Wenn die Anzahl der Elemente in der Folge ungerade ist
      --  wird einmal weniger gesprungen um das mittlere Element
      --  der Folge zu erreichen.
      if ElementCounter mod 2 > 0 then
         JumpCount := JumpCount - 1;
      end if;
      --  Wird Result zu einem Zeitpunkt aufgerufen, zu dem
      --  keine Zahlen mit Hilfe von Add_Number eingegeben wurden,
      --  so soll Result die Ausnahme No_Numbers erheben.
      if ElementCounter = 0 then
         raise No_Numbers;
      end if;
      --  Hier wird mit dem Zeiger TmpAnchor bis zum Obermedian
      --  gesprungen und der Inhalt dieser Zelle zurück gegeben.
      while JumpCount /= 0 loop
         TmpAnchor := TmpAnchor.Next;
         JumpCount := JumpCount - 1;
      end loop;

      return TmpAnchor.Inhalt;

   end Result;
     
end Median;