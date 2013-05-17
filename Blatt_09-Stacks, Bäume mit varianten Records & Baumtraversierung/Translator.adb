--  FILE:    Translator.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 9
--  VERSION: 1.0
--  DATE:    13.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 9.3: Baumtraversierung
--
--  Dieses Hauptprogramm Translator demonstriert die
--  Funktionalität des Packages Expression_Trees.
--  Der Benutzer wird zur Eingabe eines Ausdrucks in UPN
--  aufgefordert. Daraufhin wird dieser Ausdruck nach Preorder-
--  sowie nach Inorder-Notation übersetzt und jeweils das
--  Ergebnis der Berechnung angegeben.
--
-------------------------------------------------------------------

with Expression_Trees, Ada.Text_IO,
     Ada.Strings.Unbounded, 
     Ada.Strings.Unbounded.Text_IO, Ada.Strings;
use  Expression_Trees, Ada.Text_IO,
     Ada.Strings.Unbounded, 
     Ada.Strings.Unbounded.Text_IO, Ada.Strings;

procedure Translator is

   Eingabe : Unbounded_String := Null_Unbounded_String;
   Tree : Expression_Tree;
   
begin

   Put_Line ("Aufgabe 9.3: Baumtraversierung");
   Put_Line ("Bitte geben Sie einen Ausdruck in UPN an:");
   Get_Line (Eingabe);
   
   --  Ein Beispiel für zwei Klammerungen:
   --  "5.4 3.6 + 8 * 10 - 20 * 3 -"
   Parse_Postorder (To_String (Eingabe), Tree);
   New_Line;
   
   Put_Preorder (Tree);
   Put_Inorder (Tree);

   Destroy (Tree);

end Translator;
