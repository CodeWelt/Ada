--  FILE:    binary_tree_equivalence.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 10
--  VERSION: 1.0
--  DATE:    21.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 10.2: Baum-Gleichheit
--
--  Zwei Bäume werden auf Gleichheit überprüft.
--  Zu diesem Zweck wird angenommen, dass jeder Knoten
--  eines Baums Daten in Form eines Integer-Werts speichert.
--  In diesem Fall von einem Binär-Baum besitzt ein Knoten
--  genau zwei Kindknoten.
--
-------------------------------------------------------------------
package body Binary_Tree_Equivalence is

   --  FUNCTION Equals
   --
   --  Die rekursive Funktion Equals überprüft die
   --  Gleichheit zwei übergebener Bäume.
   --
   --  PARAMETERS:
   --  + Left - der linkte Baum als Binary_Tree.
   --  + Right - der rechte Baum als Binary_Tree.
   --  RETURNS:
   --  Die Funktion liefert True zurück wenn die
   --  übergebenen Bäume gleich sind, andernfalls
   --  False.
   function Equals
     (Left  : in Binary_Tree;
      Right : in Binary_Tree)
     return Boolean
   is
   begin
      --  Wenn beide verzweigungen des Binärbaums Valid sind
      --  wird an dieser Stelle fortgefahren.
      if Left /= null and Right /= null then   
         --  Nur wenn der Inhalt des linken und rechten Knotens übereinstimmen
         --  wird rekursiv das linke und rechte Paar verglichen.
         if Left.Data = Right.Data then
            return Equals (Left.Left, Right.Left) and
            Equals (Left.Right, Right.Right);
         else
            return False;
         end if;
      --  Wenn beide verzweigungen des Binärbaums null sind
      --  wird True zurückgegeben.
      elsif Left = null and Right = null then
         return True;
      else
         return False;
      end if;   
   end Equals;

end Binary_Tree_Equivalence;
