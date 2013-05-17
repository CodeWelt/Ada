--  FILE:    tree_equivalence.adb
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
--  In diesem Fall wird die Anzahl der Kindknoten durch eine
--  Liste bestimmt.
--
-------------------------------------------------------------------
package body Tree_Equivalence is

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
     (Left  : in Tree;
      Right : in Tree)
     return Boolean
   is
      LeftList_Cell_Ref, RightList_Cell_Ref : List_Cell_Ref;
      CounterLeft, CounterRight : Integer := 0;
   begin
      if Left /= null and Right /= null then   
         if Left.Data = Right.Data then
            --  Get Length of Left Tree
            --  Die Anzahl der Kinder des linken
            --  Baums wird ermittelt.
            LeftList_Cell_Ref := Left.Children;
            while LeftList_Cell_Ref /= null loop
               CounterLeft := CounterLeft + 1;
               LeftList_Cell_Ref := LeftList_Cell_Ref.Next;
            end loop;
            --  Get Length of Right Tree
            --  Die Anzahl der Kinder des rechten
            --  Baums wird ermittelt.
            RightList_Cell_Ref := Right.Children;
            while RightList_Cell_Ref /= null loop
               CounterRight := CounterRight + 1;
               RightList_Cell_Ref := RightList_Cell_Ref.Next;
            end loop;      
            --  Wenn die Bäume eine unterschiedliche Anzahl an Kinder haben
            --  sind sie ungleich.
            if CounterLeft /= CounterRight then
               return False;
            else
               --  Die Läufer werden wieder an den
               --  Anfang der Liste gesetzt um nun
               --  nocheinmal die Liste der Kinder
               --  zu durchlaufen und jedes Paar zu
               --  vergleichen.
               LeftList_Cell_Ref := Left.Children;
               RightList_Cell_Ref := Right.Children;
               --  Da beide Listen die selbe Länge haben ist es
               --  an dieser Stelle egal ob der linke oder rechte
               --  Läufer überprüft wird, ob er am Ende der Liste
               --  angekommen ist.
               while LeftList_Cell_Ref /= null loop
                  --  Es wird die Funktion rekursiv für jedes Paar von Kindern
                  --  des linken und rechten Baums aufgerufen.
                  if Equals (LeftList_Cell_Ref.Child, RightList_Cell_Ref.Child)
                  = False then
                     --  Nur im falle von False wird False zurückgegeben.
                     --  True wird dann zurückgegeben wenn kein Unterschied
                     --  aller Kinder-Paare gefunden wurde.
                     return False;
                  end if;
                  LeftList_Cell_Ref := LeftList_Cell_Ref.Next;
                  RightList_Cell_Ref := RightList_Cell_Ref.Next;
               end loop;
            end if;
         else
            return False;
         end if;
      elsif Left = null and Right = null then
         return True;
      else
         return False;
      end if;
      --  Wenn während der gesamten überprüfung kein
      --  unterschiedliches Element gefunden wurde,
      --  wird True zurückgegeben.
      --  Das kommt genau dann vor, wenn in der while
      --  Schleife die alle Kinder des Baums durchläuft
      --  nie ein unterschied festgestellt wurde.
      return True;   
   end Equals;
   
end Tree_Equivalence;