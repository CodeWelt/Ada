--  FILE:    is_acyclic.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 10
--  VERSION: 1.0
--  DATE:    21.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 10.3: Zyklenerkennung
--
--  Die Funktion erkennt durch Verwendung der Listen-Operationen
--  ob eine zyklische oder eine azyklische Liste �bergeben
--  wurde. Die Funktion gibt True zur�ck falls die leere Liste
--  �bergeben wurde.
--
-------------------------------------------------------------------
with US_Lists;
use US_Lists;

package body is_acyclic is

   --  FUNCTION Is_Acyclic
   --
   --  Die Funktion Is_Acyclic nimmt als Parameter
   --  eine Liste bei der �berpr�ft werden soll
   --  ob sie zyklisch oder azyklisch ist.
   --  Die Funktion gibt True zur�ck wenn die
   --  Liste azyklisch ist, andernfalls False.
   --
   --  PARAMETERS:
   --  + List - Die liste welche �berpr�ft werden soll.
   --
   --  RETURNS:
   --  Die Funktion gibt einen Boolean Wert zur�ck
   --  der bei eine azyklischen Liste True ist,
   --  andernfalls False.
   function Is_Acyclic
     (List : in US_Lists.List)
      return Boolean
   is
      --  Beide List_Cursor werden f�r den Anfang
      --  auf das erste Element der Liste zeigen.
      LCFirst, LCWalker : List_Cursor := First (List);
   begin
      --  Die Funktion gibt True zur�ck falls die leere
      --  Liste �bergeben wurde.
      if Is_Empty (List) then
         return True;
      end if;
      --  Die Schleife l�uft durch alle Elemente der
      --  Liste. Es wird mit der Variable LCWalker
      --  durch alle Elemente der Liste gelaufen.
      --  LCFirst zeigt immer auf das erste Element.
      loop
         Forward (LCWalker); 
         --  Wenn das aktuelle Element kein Valid
         --  Element ist, wurde das Ende der Liste
         --  erreicht. Damit ist die Liste azyklisch.
         if Is_Valid (LCWalker) = False then
            return True;
         end if;
         --  Wird beim Durchlaufen kein Ende der Liste
         --  festgestellt, wird an dieser Stelle
         --  �berpr�ft ob das aktuelle Element gleich
         --  dem Ersten Element ist. Wenn das der
         --  Fall ist, ist die Liste zyklisch.
         if LCWalker = LCFirst then
            return False;
         end if;
      end loop;        
   end Is_Acyclic;

end is_acyclic;