--  FILE:    expression_trees.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 9
--  VERSION: $Revision: 235 $
--  DATE:    $Date: 2006-12-21 18:51:44 +0100 (Thu, 21 Dec 2006) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  Bietet Funktionalitaet zum Lesen und Schreiben von arithmetischen
--  Ausdruecken
--


package Expression_Trees is

   --  TYPE Expression_Tree
   --
   --  Repraesentiert einen Binaerbaum, dessen Knoten einen
   --  arithmetischen Ausdruck darstellen
   type Expression_Tree is private;


   --  EXCEPTION Malformed_Expression
   --
   --  Wird beim Parsen eines Ausdrucks erhoben, falls der Text
   --  Syntaxfehler enthaelt
   Malformed_Expression : exception;


   --  PROCEDURE Parse_Postorder
   --
   --  Analysiert den Text 'Expression' und erzeugt daraus einen
   --  Syntaxbaum. Der Text muss in Postorder-Notation formatiert
   --  sein. Erhebt Malformed_Expression, falls der Text syntaktisch
   --  nicht korrekt ist.
   --
   --  Grammatik fuer 'Expression', Startsymbol S, es duerfen beliebig
   --  viele Leerzeichen zwischen Zahlen und Operatoren stehen:
   --  S ::= L N
   --  N ::= L O
   --      | L N O
   --      | N N
   --  O ::= '+' | '-' | '*' | '/'
   --  L steht fuer eine Zahl, die mit Float'Value (L) verarbeitet
   --  werden kann und die ausschliesslich aus den Zeichen '0'..'9'
   --  | 'e' | 'E' | '.'  besteht.
   --
   --  Beispiele: 5.3 2.3+4 6 *-
   --             4 5 + 7 * 3- 9/
   --
   --  PARAMETER:
   --  + Expression - Zu analysierender Text
   --  + Tree - Syntaxbaum, der den arithmetischen Ausdruck modelliert
   --
   --  RAISES:
   --  + Malformed_Expression - falls die 'Expression' syntaktisch
   --  nicht korrekt ist
   procedure Parse_Postorder
     (Expression : in     String;
      Tree       :    out Expression_Tree);

   --  EXCEPTION Empty_Tree
   --
   --  Wird erhoben, falls eine Operation auf dem leeren Baum
   --  ausgefuehrt wird
   Empty_Tree : exception;


   --  FUNCTION Is_Empty
   --
   --  Gibt 'True' zurueck, falls 'Tree' der leere Baum ist, sonst
   --  'False'
   function Is_Empty
     (Tree : in Expression_Tree)
     return Boolean;


   --  FUNCTION Value
   --
   --  Berechnet das Resultat eines Rechenbaums.
   --
   --  RAISES:
   --  + Empty_Tree - falls 'Tree' der leere Baum ist
   function Value
     (Tree : in Expression_Tree)
     return Float;


   --  PROCEDURE Put_Preorder
   --
   --  Gibt den arithmetischen Ausdruck in pre-order Notation (mit
   --  Funktionsaufrufen in Ada-Syntax) aus.
   --
   --  Beispiele:
   --  "-" ("+" (5.30000, 2.30000), "*" (4.00000, 6.00000))
   --  "/" ("-" ("*" ("+" (4.00000, 5.00000), 7.00000), 3.00000), 9.00000)
   --
   --  RAISES:
   --  + Empty_Tree - falls 'Tree' der leere Baum ist
   procedure Put_Preorder
     (Tree : in Expression_Tree);


   --  PROCEDURE Put_Inorder
   --
   --  Gibt den arithmetischen Ausdruck in in-order Notation aus. Gibt
   --  notwendige, jedoch keine unnoetigen Klammern aus.
   --
   --  Beispiele:
   --  5.30000 + 2.30000 - 4.00000 * 6.00000
   --  ((4.00000 + 5.00000) * 7.00000 - 3.00000) / 9.00000
   --
   --  RAISES:
   --  + Empty_Tree - falls 'Tree' der leere Baum ist
   procedure Put_Inorder
     (Tree : in Expression_Tree);


   --  PROCEDURE Destroy
   --
   --  Gibt den Speicher von 'Tree' frei und weist 'Tree' den leeren
   --  Baum zu.
   --
   --  POST: Is_Empty (Tree)
   procedure Destroy
     (Tree : in out Expression_Tree);


private

   type Node;

   type Expression_Tree is access Node;

   --  TYPE Node
   --
   --  Modelliert einen Knoten des Binaerbaums durch einen varianten
   --  Record:
   --  * Innere Knoten haben 'Kind' in { '-', '+', '*', '/' }. Sie
   --    modellieren eine Berechnung. Die beiden Unterbaeume 'Left' und
   --    'Right' werden mit der durch 'Kind' angegebenen Operation
   --    verknuepft.
   --  * Alle anderen Knoten sind Blaetter. Sie besitzen die
   --    Komponente 'Value'. Sie modellieren den Wert dieser Komponente.
   type Node (Kind : Character := '=') is
      record
         case Kind is
            when '-' | '+' | '*' | '/' =>
               Left  : Expression_Tree;
               Right : Expression_Tree;

            when others =>
               Value : Float := 0.0;
         end case;
      end record;


end Expression_Trees;
