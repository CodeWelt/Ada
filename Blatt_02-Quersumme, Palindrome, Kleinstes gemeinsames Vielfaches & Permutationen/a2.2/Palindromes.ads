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
--  Die Funktionen in diesem package Palindromes überprüfen jeweils
--  ob die als String übergebene Zeichenkette ein Palindrom ist.
--  Wenn die Zeichenkette ein Palindrom ist wird True
--  zurückgegeben, andernfalls False.
--
-------------------------------------------------------------------

package Palindromes is

   --  FUNCTION Is_Palindrome_Recursive
   --  Die Funktion überprüft ob das als String übergebene
   --  Wort ein Palindrom ist.
   --  Mit der Hilfe einer rekursiven Funktion werden die Zeichen
   --  des Wortes überprüft und entweder True oder False
   --  zurückgegeben.
   --
   --  PARAMETERS:
   --  String ist das Wort welches überprüft werden soll.
   --
   --  RETURNS: Wenn die übergebene Zeichenkette ein Palindrom ist
   --  wird True zurückgegeben, andernfalls False.
   function Is_Palindrome_Recursive
     (Word : in String)
     return Boolean;

   --  FUNCTION Is_Palindrome_For
   --  Die Funktion überprüft ob das als String übergebene
   --  Wort ein Palindrom ist.
   --  Mit der Hilfe einer For Schleife werden die Zeichen
   --  des Wortes überprüft und entweder True oder False
   --  zurückgegeben.
   --
   --  PARAMETERS:
   --  String ist das Wort welches überprüft werden soll.
   --
   --  RETURNS: Wenn die übergebene Zeichenkette ein Palindrom ist
   --  wird True zurückgegeben, andernfalls False.
   function Is_Palindrome_For
     (Word : in String)
      RETURN Boolean;

   --  FUNCTION Is_Palindrome_While
   --  Die Funktion überprüft ob das als String übergebene
   --  Wort ein Palindrom ist.
   --  Mit der Hilfe einer While Schleife werden die Zeichen
   --  des Wortes überprüft und entweder True oder False
   --  zurückgegeben.
   --
   --  PARAMETERS:
   --  String ist das Wort welches überprüft werden soll.
   --
   --  RETURNS: Wenn die übergebene Zeichenkette ein Palindrom ist
   --  wird True zurückgegeben, andernfalls False.
   function Is_Palindrome_While
     (Word : in String)
     return Boolean;

end Palindromes;
