--  FILE:    Calculator.adb
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
--  Das Programm Calculator verwendet das Packet fractions um
--  einen einfachen Taschenrechner zu simulieren.
--
-------------------------------------------------------------------

with Ada.Text_IO, Fractions, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO;

procedure Calculator
is
   ANum, BNum : Integer := 0;
   ADenom, BDenom : Integer := 1;
   
   RunOnce : Boolean := True;
   AFrac : Fractions.Fraction;
   BFrac : Fractions.Fraction;
   Ergebnis : Fractions.Fraction;
   Eingabe : Character := '?';
begin

   Put ("Taschenrechner v1.0");
   New_Line;
   Put ("Bitte geben Sie einen Bruch ein (Zähler): ");
   Get (ANum);
   
   Put ("Bitte geben Sie einen Bruch ein (Nenner): ");
   Get (ADenom);
   
   --  Aus dem gerade eingegebenen Zähler und Nenner wird
   --  mit To_Fraction aus dem Packet fractions ein Bruch
   --  gemacht.
   AFrac := Fractions.To_Fraction (ANum, ADenom);

   --  Die Schleife läuft solange bis der Benutzer ein '=' zum
   --  beenden des Taschenrechners eingibt.
   while Eingabe /= '=' loop

      --  Für die erste Berechnung wird der vor der Schleife
      --  eingegebene Bruch verwendet, ansonsten wird das Ergebnis
      --  der letzten Berechnung verwendet.
      if RunOnce /= True then
         AFrac := Ergebnis;
      end if;
   
      Eingabe := '?';
      --  Der Benutzer gibt einer der vier Grundrechenarten
      --  ('+', '-', '*', '/') ein, oder '=' um zu beenden.
      while Eingabe not in '*' .. '+' and Eingabe /= '-' and Eingabe /= '/'
      and Eingabe /= '=' loop
         Put ("Bitte geben Sie einer der vier Grundrechenarten");
         New_Line;
         Put ("('+', '-', '*', '/') ein, oder '=' um zu beenden: ");
         Get (Eingabe);
      end loop;

      --  Wenn kein '=' eingegeben wurde, wird der Benutzer
      --  aufgefordert einen zweiten Operand einzugeben.
      if Eingabe /= '=' then
         Put ("Bitte geben Sie einen Bruch ein (Zähler): ");
         Get (BNum);
         Put ("Bitte geben Sie einen Bruch ein (Nenner): ");
         Get (BDenom);
         --  Aus dem gerade eingegebenen Zähler und Nenner wird
         --  mit To_Fraction aus dem Packet fractions ein Bruch
         --  gemacht.
         BFrac := Fractions.To_Fraction (BNum, BDenom);
      end if;
      
      --  Jenachdem welche Grundrechenarten vom Benutzer eingegeben
      --  wurde, wird die dazugehörige Funktion im Packet fractions
      --  mit den Beiden Operanden als Parameter aufgerufen.
      if Eingabe = '+' then
         Ergebnis := Fractions."+" (AFrac, BFrac);
      elsif Eingabe = '-' then
         Ergebnis := Fractions."-" (AFrac, BFrac);
      elsif Eingabe = '*' then
         Ergebnis := Fractions."*" (AFrac, BFrac);
      elsif Eingabe = '/' then
         Ergebnis := Fractions."/" (AFrac, BFrac);
      end if;
      
      if Eingabe /= '=' then
         Put ("Ergebnis: " & Fractions.Image (AFrac) &
          " " & Eingabe & " " & Fractions.Image (BFrac) & " = " &
          Fractions.Image (Ergebnis));
         New_Line;
         New_Line;
      end if;
      RunOnce := False;
   end loop;
 
exception
   when Fractions.Division_By_Zero =>
      Put ("Exception: Division_By_Zero. Durch Null darf nicht geteilt" &
      " werden.");
      New_Line;
      Put ("Der Taschenrechner wird neu gestartet. Bitte einen anderen Bruch" &
      " wählen.");
      New_Line;
      New_Line;
      Calculator;
   when Constraint_Error =>
      Put ("Exception: Constraint_Error.");
      New_Line;
      Put ("Die Berechnung konnte nicht ohne Ueberlauf durchgefuehrt werden.");
      New_Line;
      Put ("Der Taschenrechner wird neu gestartet. Bitte einen anderen Bruch" &
      " wählen.");
      New_Line;
      New_Line;
      Calculator;
      
end Calculator;
