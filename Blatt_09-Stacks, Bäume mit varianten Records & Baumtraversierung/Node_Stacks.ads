--  FILE:    Node_Stacks.ads
--  PROJECT: Programmieruebungen, Uebungsblatt 9
--  VERSION: 1.0
--  DATE:    13.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 9.3: Baumtraversierung
--
--  Das Package bietet Funktionalität für einen Stack
--  der Elemente vom Typ Expression_Trees.Expression_Tree
--  verwaltet.
--
-------------------------------------------------------------------
with Expression_Trees;

package Node_Stacks is
   Stack_Underflow : exception;

   type Stack_Cell is private;
   type Ref_Stack is access Stack_Cell;

   procedure Push (S : in out Ref_Stack; ET : Expression_Trees.Expression_Tree);
   

   function Top (S : in Ref_Stack) return Expression_Trees.Expression_Tree;
   function Pop (S : in Ref_Stack; Free : in Boolean) return Ref_Stack;
   

   procedure Empty (S : in out Ref_Stack);
   function IsEmptyStack (S : in Ref_Stack) return Boolean;


   private
   type Stack_Cell is record
      ET             : Expression_Trees.Expression_Tree;
      Next       : Ref_Stack;
   end record;


end Node_Stacks;