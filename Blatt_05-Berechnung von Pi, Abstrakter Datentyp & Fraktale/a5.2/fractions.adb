--  FILE:    fractions.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 5
--  VERSION: 1.0
--  DATE:    02.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 5.2: Abstrakter Datentyp
--
--  Ein abstrakter Datentyp ist ein Typ dessen sichtbare
--  Eigenschaftern ausschließlich duch eine Menge von
--  Unterprogrammen definiert sind. Ein ADT wird in Ada innerhalb
--  einer Paket-Spezifikation als privater Typ deklariert.
--  Der Typ Fraction soll einen mathematischen Bruch modellieren.
--  Die Komponente Numerator speichert den Zähler des Bruchs,
--  Denominator den Nenner.
--
-------------------------------------------------------------------

with Ada.Strings.Unbounded, Ada.Strings;
use  Ada.Strings.Unbounded, Ada.Strings;

package body Fractions is

   --  FUNCTION GgT
   --  Die rekursive Funktion berechnet den größten gemeinsamen
   --  Teiler der vom Benutzer eingegebenen zwei Zahlen.
   --
   --  PARAMETERS:
   --  Die Integer Parameter ErsteZahl und ZweiteZahl wurden
   --  vom Benutzer eingegeben und an die Funktion übergeben.
   --
   --  RETURNS: Die Funktion gibt den größten gemeinsamen
   --  Teiler der beiden übergebenen Zahlen als Integer zurück.
   function GgT
      (ErsteZahl, ZweiteZahl : Integer)
      return Integer
   is
   begin
      if ErsteZahl = ZweiteZahl then
         return ErsteZahl;         
      elsif ErsteZahl > ZweiteZahl then
         return GgT (ErsteZahl - ZweiteZahl, ZweiteZahl);
      else
         return GgT (ZweiteZahl - ErsteZahl, ErsteZahl);
      end if;      
   end GgT;

   --  FUNCTION LCM
   --  Das kleinste gemeinsame Vielfache ist die erste Zahl multipliziert
   --  mit der Zweiten geteilt durch den größten gemeinsamen Teiler
   --  dieser zwei Zahlen. Beispiel: (a * b) / GgT(a, b)
   --  
   --  PARAMETERS:
   --  ErsteZahl und ZweiteZahl sind die Zahlen als Integer wert
   --  deren kleinste gemeinsame Vielfache bestimmt werden soll.
   --
   --  RETURNS: Die Funktion gibt das kleinste gemeinsame Vielfache
   --  der beiden Zahlen als Integer zurück.
   function LCM
      (ErsteZahl, ZweiteZahl : Integer)
      return Integer
   is
   begin
      return (ErsteZahl * ZweiteZahl) / GgT (ErsteZahl, ZweiteZahl);
   end LCM;

   --  FUNCTION Kuerzen
   --  Die Funktion Kuerzen kuerzt einen gegebenen Bruch vollständig.
   --
   --  PARAMETERS:
   --  Item: Der Bruch wird als Typ Fraction übergeben.
   --
   --  RETURNS: Die Funktion gibt den vollständig gekürzten Bruch
   --  als Typ Fraction zurück.
   function Kuerzen
     (Item : in Fraction)
     return Fraction
   is
      Temp : Fraction;
      Kuerz : Fraction;
      Zero : Fraction;
      ggT : Integer;
      Negativ : Boolean := False;
   begin
      if Item.Numerator = 0 or Item.Denominator = 0 then
         return Zero;
      end if;
      
      Temp := Item;
      --  Hier wird auf das Vorzeichen geachtet um in der Berechnung
      --  mögliche Überläufe zu vermeiden.
      if Temp.Numerator < 0 then
         Negativ := True;
         Temp.Numerator := Temp.Numerator * (-1);
      end if;
      
      ggT := Fractions.GgT (Temp.Numerator, Temp.Denominator);
      
      Kuerz.Numerator := Temp.Numerator / ggT;
      Kuerz.Denominator := Temp.Denominator / ggT;
      
      if Negativ = True then
         Kuerz.Numerator := Kuerz.Numerator * (-1);
      end if;

      return Kuerz;
   
   end Kuerzen;
   
   --  FUNCTION To_Fraction
   --
   --  Berechnet aus Werten fuer Zaehler und Nenner einen Bruch.
   --
   --  PARAMETERS:
   --  + Numerator: Zaehler
   --  + Denominator: Nenner
   --
   --  RAISES: * Division_By_Zero - falls Denominator = 0
   --          * Constraint_Error - falls Denominator = Integer'First
   function To_Fraction
     (Numerator   : in Integer;
      Denominator : in Integer)
     return Fraction
   is
      Returnme : Fraction;
   begin
      --  Durch Null darf nicht geteilt werden.
      if Denominator = 0 then
         raise Division_By_Zero;
      end if;
      
      if Denominator < 0 then
         Returnme.Numerator := Numerator * (-1);
         Returnme.Denominator := Denominator * (-1);         
      else
         Returnme.Numerator := Numerator;
         Returnme.Denominator := Denominator;
      end if;
      return Returnme;
   end To_Fraction;

   --  FUNCTION "+"
   --
   --  Additionsoperator.
   --
   --  RAISES: * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "+"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction
   is
      Returnme : Fraction;
      LCMx : Integer;
      Leftx, Rightx : Fraction;
   begin
      LCMx := LCM (Left.Denominator, Right.Denominator);
      
      Leftx := Left;
      Rightx := Right;
      
      --  Die beiden zu addierenden Brüche werden auf einen gemeinsamen
      --  Nenner gebracht und schließlich die beiden Zähler addiert.
      Leftx.Numerator := Leftx.Numerator * (LCMx / Left.Denominator);
      Leftx.Denominator := Leftx.Denominator * (LCMx / Left.Denominator);
      
      Rightx.Numerator := Rightx.Numerator * (LCMx / Right.Denominator);
      Rightx.Denominator := Rightx.Denominator * (LCMx / Right.Denominator);
      
      Returnme.Numerator := Leftx.Numerator + Rightx.Numerator;
      Returnme.Denominator := Leftx.Denominator;
      --  Das Ergebnis wird gekürzt und zurückgegeben.
      return Kuerzen (Returnme);
   end "+";

   --  FUNCTION "-"
   --
   --  Subtraktionsoperator.
   --
   --  RAISES: * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "-"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction
   is
      Returnme : Fraction;
      LCMx : Integer;
      Leftx, Rightx : Fraction;
   begin
      LCMx := LCM (Left.Denominator, Right.Denominator);
      
      Leftx := Left;
      Rightx := Right;
      
      --  Ähnlich wie bei der function "+" werden die Brüche auf einen
      --  gemeinsamen Nenner gebracht und schließlich die
      --  beiden Zähler subtrahiert.
      Leftx.Numerator := Leftx.Numerator * (LCMx / Left.Denominator);
      Leftx.Denominator := Leftx.Denominator * (LCMx / Left.Denominator);
      
      Rightx.Numerator := Rightx.Numerator * (LCMx / Right.Denominator);
      Rightx.Denominator := Rightx.Denominator * (LCMx / Right.Denominator);
      
      Returnme.Numerator := Leftx.Numerator - Rightx.Numerator;
      Returnme.Denominator := Leftx.Denominator;
      --  Das Ergebnis wird gekürzt und zurückgegeben.
      return Kuerzen (Returnme);
   end "-";

   --  FUNCTION "*"
   --
   --  Multiplikationsoperator.
   --
   --  RAISES: * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "*"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction
   is
      Returnme : Fraction;
   begin
      --  Die beiden Zähler und Nenner werden multipliziert.
      Returnme.Numerator := Left.Numerator * Right.Numerator;
      Returnme.Denominator := Left.Denominator * Right.Denominator;
      --  Das Ergebnis wird gekürzt und zurückgegeben.
      return Kuerzen (Returnme);

   end "*";

   --  FUNCTION "/"
   --
   --  Divisionsoperator.
   --
   --  RAISES: * Division_By_Zero - falls Right den Wert 0 hat.
   --          * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "/"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction
   is
      Returnme : Fraction;
   begin
      --  Durch Null darf nicht geteilt werden.
      if Right.Denominator = 1 and Right.Numerator = 0 then
         raise Division_By_Zero;
      end if;
      
      --  Hier wird der Kehrwert gebildet und das Ergebnis dann
      --  multipliziert. Es wird auf das Vorzeichen geachtet um in der
      --  Berechnung mögliche Überläufe zu vermeiden.
      Returnme.Numerator := Left.Numerator * Right.Denominator;
      
      if Right.Numerator < 0 then
         Returnme.Numerator := Returnme.Numerator * (-1);
         Returnme.Denominator := Left.Denominator * (Right.Numerator * (-1));
      else
         Returnme.Denominator := Left.Denominator * Right.Numerator;
      end if;
      --  Das Ergebnis wird gekürzt und zurückgegeben.
      return Kuerzen (Returnme);

   end "/";

   --  FUNCTION Image
   --
   --  Wandelt einen Bruch in eine Zeichenkette um. Der Bruch ist
   --  vollstaendig gekuerzt. Die Zeichenkette hat meist das Format
   --  <zaehler>'/'<nenner>, wobei <zaehler> evtl. negativ sein kann,
   --  <nenner> jedoch nicht. Hat der Nenner den Wert 1, so wird der
   --  hintere Teil ('/'<nenner>) nicht an die Zeichenkette
   --  angehaengt. Die zurueckgegebene Zeichenkette enthaelt keine
   --  Leerzeichen.
   function Image
     (Item : in Fraction)
     return String
   is
      Koncatenation, NoSpaces : Unbounded_String := Null_Unbounded_String;
   begin
      
      --  Hat der Nenner den Wert 1, so wird der hintere Teil
      --  ('/'<nenner>) nicht an die Zeichenkette angehaengt.
      if Item.Denominator = 1 then
         Koncatenation := Koncatenation & Item.Numerator'Img;
      else
         Koncatenation := Koncatenation & Item.Numerator'Img & "/"
         & Item.Denominator'Img;
      end if;
      
      --  Die Schleife läuft für jedes Zeichen im Unbounded_String
      --  Koncatenation. Nur wenn das Zeichen kein Leerzeichen ist,
      --  wird es an den Unbounded_String NoSpaces angehängt.
      --  Die zurueckgegebene Zeichenkette enthaelt keine Leerzeichen.
      for Laufvar in 1 .. Length (Koncatenation) loop
         if Element (Koncatenation, Laufvar) /= ' ' then
            NoSpaces := NoSpaces & Element (Koncatenation, Laufvar);
         end if;
      end loop;
    
      return To_String (NoSpaces);
   end Image;

end Fractions;