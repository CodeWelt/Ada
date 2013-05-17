--  FILE:    Node_Stacks.adb
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

package body Node_Stacks is


   --  PROCEDURE Push
   --
   --  Die Prozedur Push erstellt eine neue Stack_Cell
   --  im übergebenen Ref_Stack und weist der neuen
   --  Zelle das übergebene Element zu.
   --  
   --  PARAMETERS:
   --  + S : Pointer auf den Stack.
   --  + ET : Element welches auf den Stack geschoben werden
   --  soll.
   procedure Push (S : in out Ref_Stack; ET : in Expression_Trees.Expression_Tree) is
   begin
      S := new Stack_Cell'(ET, S);
   end Push;


   --  FUNCTION Top
   --
   --  Die Funktion Top gibt den Inhalt des aktuell
   --  ganz oben auf dem Stack liegenden Elements zurück.
   --  
   --  PARAMETERS:
   --  + S : Pointer auf den Stack.
   --  RETURNS:
   --  Die Funktion gibt den Inhalt des aktuell ersten
   --  Elements des Stacks.
   function Top (S : in Ref_Stack) return Expression_Trees.Expression_Tree is
   begin
      if IsEmptyStack (S) then
         raise Stack_Underflow;
      end if;
      return S.ET;
   end Top;

   --  FUNCTION Pop
   --
   --  Die Funktion Pop nimmt das aktuell ganz oben
   --  auf dem Stack liegende Element weg vom Stack.
   --  der Ref_Stack zeigt nun auf das darunter
   --  liegende Element.
   --  
   --  PARAMETERS:
   --  + S : Pointer auf den Stack.
   --  RETURNS:
   --  Die Funktion gibt einen Zeiger auf das darunter
   --  liegende Element zurück.
   function Pop (S : in Ref_Stack; Free : in Boolean) return Ref_Stack is
   begin
      if IsEmptyStack (S) then
         raise Stack_Underflow;
      end if;
      return S.Next;
   end Pop;

   --  PROCEDURE Empty
   --
   --  Die Prozedur Empty leert den Stack indem der
   --  Ref_Stack pointer auf Null gesetzt wird.
   --  
   --  PARAMETERS:
   --  + S : Pointer auf den Stack.
   procedure Empty (S : in out Ref_Stack) is
   begin
      S := null;
   end Empty;

   --  FUNCTION IsEmptyStack
   --
   --  Die Funktion IsEmptyStack überpfrüft ob der
   --  Ref_Stack auf keine Zelle weist. Wenn
   --  das der Fall ist wird True zurückgegeben,
   --  andernfalls False.
   --  
   --  PARAMETERS:
   --  + S : Pointer auf den Stack.
   --  RETURNS:
   --  Die Funktion gibt zurück ob der Stack
   --  leer ist oder nicht.
   function IsEmptyStack (S : in Ref_Stack) return Boolean is
   begin
      return S = null;
   end IsEmptyStack;

end Node_Stacks;