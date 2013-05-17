--  FILE:    fractions.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 5
--  VERSION: $Revision: 165 $
--  DATE:    $Date: 2006-11-24 14:54:47 +0100 (Fri, 24 Nov 2006) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  PACKAGE Fractions
--
--  Bietet einen abstrakten Datentyp fuer Rechnungen mit Bruechen an.
--

package Fractions is

   --  TYPE Fraction
   --
   --  Abstrakter Datentyp zur Modellierung von Bruechen. Variablen
   --  ohne explizite Initialisierung werden implizit auf den Wert 0
   --  gesetzt.
   type Fraction is private;

   --  EXCEPTION Division_By_Zero
   --
   --  Wird erhoben, wann immer der Versuch unternommen wird, durch 0
   --  zu dividieren.
   Division_By_Zero : exception;

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
     return Fraction;

   --  FUNCTION "+"
   --
   --  Additionsoperator.
   --
   --  RAISES: * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "+"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction;

   --  FUNCTION "-"
   --
   --  Subtraktionsoperator.
   --
   --  RAISES: * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "-"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction;

   --  FUNCTION "*"
   --
   --  Multiplikationsoperator.
   --
   --  RAISES: * Constraint_Error - falls die Berechnung nicht ohne
   --          Ueberlauf durchgefuehrt werden kann.
   function "*"
     (Left  : in Fraction;
      Right : in Fraction)
     return Fraction;

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
     return Fraction;

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
     return String;

private

   --  DARSTELLUNG des Typs Fraction
   --
   --  Brueche werden mit Zaehler und Nenner dargestellt. Sie werden
   --  immer vollstaendig gekuerzt.  Die Zahl 0 wird mit Nenner 1
   --  dargestellt.
   type Fraction is
      record
         --  Zaehler, kann negativ werden
         Numerator   : Integer := 0;
         --  Nenner
         Denominator : Positive := 1;
      end record;

end Fractions;
