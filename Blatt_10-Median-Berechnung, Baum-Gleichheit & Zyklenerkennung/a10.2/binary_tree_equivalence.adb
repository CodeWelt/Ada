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
--  Zwei B�ume werden auf Gleichheit �berpr�ft.
--  Zu diesem Zweck wird angenommen, dass jeder Knoten
--  eines Baums Daten in Form eines Integer-Werts speichert.
--  In diesem Fall von einem Bin�r-Baum besitzt ein Knoten
--  genau zwei Kindknoten.
--
-------------------------------------------------------------------
package body Binary_Tree_Equivalence is

   --  FUNCTION Equals
   --
   --  Die rekursive Funktion Equals �berpr�ft die
   --  Gleichheit zwei �bergebener B�ume.
   --
   --  PARAMETERS:
   --  + Left - der linkte Baum als Binary_Tree.
   --  + Right - der rechte Baum als Binary_Tree.
   --  RETURNS:
   --  Die Funktion liefert True zur�ck wenn die
   --  �bergebenen B�ume gleich sind, andernfalls
   --  False.
   function Equals
     (Left  : in Binary_Tree;
      Right : in Binary_Tree)
     return Boolean
   is
   begin
      --  Wenn beide verzweigungen des Bin�rbaums Valid sind
      --  wird an dieser Stelle fortgefahren.
      if Left /= null and Right /= null then   
         --  Nur wenn der Inhalt des linken und rechten Knotens �bereinstimmen
         --  wird rekursiv das linke und rechte Paar verglichen.
         if Left.Data = Right.Data then
            return Equals (Left.Left, Right.Left) and
            Equals (Left.Right, Right.Right);
         else
            return False;
         end if;
      --  Wenn beide verzweigungen des Bin�rbaums null sind
      --  wird True zur�ckgegeben.
      elsif Left = null and Right = null then
         return True;
      else
         return False;
      end if;   
   end Equals;

end Binary_Tree_Equivalence;
