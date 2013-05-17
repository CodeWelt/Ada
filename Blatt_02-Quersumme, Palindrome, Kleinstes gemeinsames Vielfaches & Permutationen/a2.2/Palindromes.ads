--  FILE:    Palindromes.ads
--  PROJECT: Programmieruebungen, Uebungsblatt 2
--  VERSION: 1.0
--  DATE:    10.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 2.2: Palindrome
--
--  Die Funktionen in diesem package Palindromes �berpr�fen jeweils
--  ob die als String �bergebene Zeichenkette ein Palindrom ist.
--  Wenn die Zeichenkette ein Palindrom ist wird True
--  zur�ckgegeben, andernfalls False.
--
-------------------------------------------------------------------

package Palindromes is

   --  FUNCTION Is_Palindrome_Recursive
   --  Die Funktion �berpr�ft ob das als String �bergebene
   --  Wort ein Palindrom ist.
   --  Mit der Hilfe einer rekursiven Funktion werden die Zeichen
   --  des Wortes �berpr�ft und entweder True oder False
   --  zur�ckgegeben.
   --
   --  PARAMETERS:
   --  String ist das Wort welches �berpr�ft werden soll.
   --
   --  RETURNS: Wenn die �bergebene Zeichenkette ein Palindrom ist
   --  wird True zur�ckgegeben, andernfalls False.
   function Is_Palindrome_Recursive
     (Word : in String)
     return Boolean;

   --  FUNCTION Is_Palindrome_For
   --  Die Funktion �berpr�ft ob das als String �bergebene
   --  Wort ein Palindrom ist.
   --  Mit der Hilfe einer For Schleife werden die Zeichen
   --  des Wortes �berpr�ft und entweder True oder False
   --  zur�ckgegeben.
   --
   --  PARAMETERS:
   --  String ist das Wort welches �berpr�ft werden soll.
   --
   --  RETURNS: Wenn die �bergebene Zeichenkette ein Palindrom ist
   --  wird True zur�ckgegeben, andernfalls False.
   function Is_Palindrome_For
     (Word : in String)
      RETURN Boolean;

   --  FUNCTION Is_Palindrome_While
   --  Die Funktion �berpr�ft ob das als String �bergebene
   --  Wort ein Palindrom ist.
   --  Mit der Hilfe einer While Schleife werden die Zeichen
   --  des Wortes �berpr�ft und entweder True oder False
   --  zur�ckgegeben.
   --
   --  PARAMETERS:
   --  String ist das Wort welches �berpr�ft werden soll.
   --
   --  RETURNS: Wenn die �bergebene Zeichenkette ein Palindrom ist
   --  wird True zur�ckgegeben, andernfalls False.
   function Is_Palindrome_While
     (Word : in String)
     return Boolean;

end Palindromes;
