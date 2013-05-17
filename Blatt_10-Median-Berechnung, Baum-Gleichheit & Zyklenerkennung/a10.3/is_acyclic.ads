--  FILE:    is_acyclic.ads
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

package is_acyclic is

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
   return Boolean;

end is_acyclic;