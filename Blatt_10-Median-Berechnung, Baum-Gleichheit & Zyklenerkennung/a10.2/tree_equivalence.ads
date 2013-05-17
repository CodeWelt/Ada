--  FILE:    tree_equivalence.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 10
--  VERSION: $Revision: 252 $
--  DATE:    $Date: 2007-01-12 17:04:17 +0100 (Fri, 12 Jan 2007) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------


package Tree_Equivalence is

   type Tree is private;

   function Equals
     (Left  : in Tree;
      Right : in Tree)
     return Boolean;


private

   type List_Cell;

   type List_Cell_Ref is access List_Cell;


   type Tree_Node is
      record
         Data     : Integer;
         Children : List_Cell_Ref;
      end record;


   type List_Cell is
      record
         Child : Tree;
         Next  : List_Cell_Ref;
      end record;


   type Tree is access Tree_Node;

end Tree_Equivalence;
