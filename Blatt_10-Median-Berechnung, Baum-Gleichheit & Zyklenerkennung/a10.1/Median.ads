--  FILE:    Median.ads
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
package Median is

   type Zeiger is private;

   --  PROCEDURE Reset
   --
   --  Es werden die bereits eingefügten Zahlen vergessen und
   --  erneut mit der leeren Folge gestartet.
   procedure Reset;

   --  PROCEDURE Add_Number
   --
   --  Die übergebene Zahl wird mit Hilfe
   --  von der Funktion Sort_In sortiert
   --  in die Liste Anchor eingefügt.
   --  PARAMETERS:
   --  Number : Die Zahl als Integer Wert die
   --  sortiert eingefügt werden soll.   
   procedure Add_Number
     (Number : in Integer);

   --  EXCEPTION No_Numbers
   --  Wird Result zu einem Zeitpunkt aufgerufen, zu dem
   --  keine Zahlen mit Hilfe von Add_Number eingegeben wurden,
   --  so soll Result die Ausnahme No_Numbers erheben.
   No_Numbers : exception;
   
   --  FUNCTION Result
   --
   --  Es wird der Obermedian berechnet und zurück gegeben.
   --
   --  RETURNS:
   --  Die Funktion liefert die Zahl als Integer Wert zurück,
   --  die den Obermedian der Liste bezeichnet.
   function Result
     return Integer;

private

   type Zelle;
   type Zeiger is access Zelle;
   type Zelle is record
      Inhalt : Integer;
      Next   : Zeiger;
   end record;

end Median;