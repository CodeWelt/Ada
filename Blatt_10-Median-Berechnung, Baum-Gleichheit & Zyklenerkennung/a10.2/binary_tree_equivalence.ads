--  FILE:    binary_tree_equivalence.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 10
--  VERSION: $Revision: 252 $
--  DATE:    $Date: 2007-01-12 17:04:17 +0100 (Fri, 12 Jan 2007) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------


package Binary_Tree_Equivalence is

   type Binary_Tree is private;

   function Equals
     (Left  : in Binary_Tree;
      Right : in Binary_Tree)
     return Boolean;


private

   type Binary_Tree_Node;

   type Binary_Tree is access Binary_Tree_Node;

   type Binary_Tree_Node is
      record
         Data  : Integer;
         Left  : Binary_Tree;
         Right : Binary_Tree;
      end record;

end Binary_Tree_Equivalence;
