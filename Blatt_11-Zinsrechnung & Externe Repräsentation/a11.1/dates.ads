--  FILE:    dates.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 11
--  VERSION: $Revision: 275 $
--  DATE:    $Date: 2007-01-19 17:43:44 +0100 (Fri, 19 Jan 2007) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  Paket zur Verwaltung eines Datums fuer Zinsberechnungen. Das Datum
--  hat die besondere Eigenschaft die Laenge eines Montas auf 30 Tage
--  zu definieren. Ein Jahr hat folglich 12*30 Tage. Der Typ 'Date'
--  kann alle Tage von 1.1.0 -- 30.12.9999 darstellen. Alle
--  Unterprogramme erheben die Ausnahme Constraint_Error, falls das
--  Resultat einer Berechnung ausserhalb dieses Bereiches faellt.
--
--  Die Unterprogramme in diesem Paket bilden auch nicht existierende
--  Tage wie z.B. den 30.2.jjjj nach.
--


package Dates is


   --  TYPE Date
   --
   --  Repraesentiert ein Datum in dem 30/360 Tage Modell (Tage pro
   --  Monat/Tage pro Jahr). Es wird keinerlei Unterstuetzung fuer im
   --  Kalender nicht existierende Tage, Wochenenden oder Feiertage
   --  angeboten.
   type Date is private;

   subtype Day_Range is Positive range 1 .. 30;
   subtype Month_Range is Positive range 1 .. 12;
   subtype Year_Range is Natural range 0 .. 9999;


   --  FUNCTION Create
   --
   --  Erzeugt ein Datum aus den Einzelwerten fuer Tag, Monat und
   --  Jahr. Wird ein zu grosser oder zu kleiner Wert uebergeben
   --  (z.B. der Tag 31, der Monat 13 oder der Tag -2), so wird der
   --  Uebertrag korrekt verarbeitet. Dadurch kann allerdings auch ein
   --  nicht existierendes Datum wie der 30.2.jjjj getroffen werden.
   --  Bei Ueberlaufen wird Constraint_Error erhoben.
   function Create
     (Day   : in Integer;
      Month : in Integer;
      Year  : in Integer)
     return Date;

   --  FUNCTION Value
   --
   --  Erzeugt ein Datum aus einem String.  Der String muss das Format
   --  TT.MM.JJJJ haben mit T,M,J in { 0-9}, andernfalls erhebt
   --  Constraint_Error. Behandelt evtl. notwendige Uebertrage analog
   --  zu der Funktion 'Create'.
   function Value
     (S : in String)
     return Date;


   --  FUNCTION Day
   --
   --  Ermittelt den Wert der Tages-Komponente eines Datums.
   function Day
     (D : in Date)
     return Day_Range;

   --  FUNCTION Month
   --
   --  Ermittelt den Wert der Monats-Komponente eines Datums.
   function Month
     (D : in Date)
     return Month_Range;

   --  FUNCTION Year
   --
   --  Ermittelt den Wert der Jahres-Komponente eines Datums.
   function Year
     (D : in Date)
     return Year_Range;

   --  FUNCTION Image
   --
   --  Gibt eine textuelle Repraesentation eines Datums zurueck. Das
   --  Format ist "TT.MM.JJJJ", mit T,M,J in {0-9}.
   function Image
     (D : in Date)
     return String;


   --  FUNCTION End_Of_Month
   --
   --  Berechnet den letzten Tag in dem Monat von 'D'. Dies ist stets
   --  der 30. des Monats.
   --
   --  RETURNS: Create (30, Month (D), Year (D))
   function End_Of_Month
     (D : in Date)
     return Date;

   --  FUNCTION End_Of_Year
   --
   --  Berechnet den letzten Tag in dem Jahr von 'D'. Dies ist stets
   --  der 30.12. des Jahres.
   --
   --  RETURNS: Create (30, 12, Year (D))
   function End_Of_Year
     (D : in Date)
     return Date;

   --  FUNCTION Earliest
   --
   --  Gibt den Wert von entweder 'Left' oder 'Right' zurueck,
   --  abhaengig davon, welches Datum frueher ist.
   --
   --  RETURNS: Left,  falls Left <= Right
   --           Right, falls Right < Left
   function Earliest
     (Left  : in Date;
      Right : in Date)
     return Date;

   --  OPERATOR +
   --
   --  Berechnet das Datum, das 'Days' Tage nach 'Left' kommt.
   --
   --  PARAMETERS:
   --  + Left - Ausgangsdatum
   --  + Days - Anzahl Tage, um die von 'Left' an weiter gezaehlt
   --  werden soll
   function "+"
     (Left  : in Date;
      Days  : in Integer)
     return Date;

   --  OPERATOR -
   --
   --  Berechnet das Datum, das 'Days' Tage vor 'Left' kommt.
   --
   --  PARAMETERS:
   --  + Left - Ausgangsdatum
   --  + Days - Anzahl Tage, um die von 'Left' an zurueck gezaehlt
   --  werden soll
   function "-"
     (Left  : in Date;
      Days  : in Integer)
     return Date;

   --  OPERATOR -
   --
   --  Berechnet die Dauer von 'Right' nach 'Left' als Anzahl von
   --  Tagen. Z.B. Create (3, 1, 2004) - Create (1, 1, 2004) = 2
   function "-"
     (Left  : in Date;
      Right : in Date)
     return Integer;


   ---------------------------------------------------------------
   -- Vergleichsoperatoren                                      --
   -- Fuer die folgenden Operatoren gilt: ein Datum ist < einem --
   -- anderen, falls es im Kalender frueher ist.                --
   ---------------------------------------------------------------

   --  implizit deklariert:
   --  function "="
   --    (Left, Right : in Date)
   --    return Boolean;
   --  function "/="
   --    (Left, Right : in Date)
   --    return Boolean;

   function "<"
     (Left  : in Date;
      Right : in Date)
     return Boolean;

   function "<="
     (Left  : in Date;
      Right : in Date)
     return Boolean;

   function ">"
     (Left  : in Date;
      Right : in Date)
     return Boolean;

   function ">="
     (Left  : in Date;
      Right : in Date)
     return Boolean;


private

   type Date is new Integer range 0 .. (9999 * 12 + 11) * 30 + 29;

end Dates;
