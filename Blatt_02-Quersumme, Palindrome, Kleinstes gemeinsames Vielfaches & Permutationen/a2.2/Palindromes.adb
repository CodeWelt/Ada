--  FILE:    Palindromes.adb
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

WITH Ada.Strings, Ada.Strings.Unbounded;
USE  Ada.Strings, Ada.Strings.Unbounded;

PACKAGE BODY Palindromes IS

RunOnce : Boolean := True;

   -------------------------
   -- Loesungsalgorithmen --
   -------------------------

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
   FUNCTION Is_Palindrome_Recursive
      (Word : IN String)
      RETURN Boolean
   IS       
      Word_Unbounded : Unbounded_String := Null_Unbounded_String; 
   BEGIN
      -- Das als Parameter übergebene Wort wird zu einem Unbounded_String konvertiert.
      Word_Unbounded := Ada.Strings.Unbounded.To_Unbounded_String(Word);

      IF RunOnce = True THEN   -- Beim ersten Durchlauf werden keine
         RunOnce := False;     -- Zeichen entfernt.
      ELSE
         -- Wenn die Zeichenkette länger als 2 Zeichen ist wird das Erste
         -- und das Letzte Zeichen entfernt.
         IF Length(Word_Unbounded) > 2 THEN        
            Delete (Word_Unbounded, Length(Word_Unbounded), Length(Word_Unbounded));
            Delete (Word_Unbounded, 1, 1);
         -- Wenn keine Zeichen mehr zum Vergleichen vorhanden sind
         -- wird True zurückgegeben.
         ELSE IF Word_Unbounded = Null_Unbounded_String THEN
            RETURN True;
         ELSE
            RETURN True;
         END IF;
         END IF;
      END IF;

      -- Das Erste und das Letzte Zeichen wird verglichen.
      IF Element(Word_Unbounded, 1) /= Element(Word_Unbounded, Length(Word_Unbounded)) THEN
         RETURN False;
      ELSE
         IF Is_Palindrome_Recursive(Ada.Strings.Unbounded.To_String(Word_Unbounded)) = False THEN
            RETURN False;
         ELSE
            RETURN True;    
         END IF;
      END IF; 
      
   END Is_Palindrome_Recursive;


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
   FUNCTION Is_Palindrome_For
      (Word : IN String)
      RETURN Boolean
   IS
   BEGIN
      -- Die For Schleife läuft für jedes Zeichen der übergebenen Zeichenkette.
      FOR Index IN Word'First..Word'Last LOOP
         -- Das Erste und das Letzte Zeichen wird verglichen.
         IF Word(Index) /= Word(Word'Last - Index + 1) THEN
            RETURN False;
         END IF;     
      END LOOP;

      RETURN True;
   END Is_Palindrome_For;


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
   FUNCTION Is_Palindrome_While
      (Word : IN String)
      RETURN Boolean
   IS
      Index : Integer := 0;
   BEGIN
      Index := Word'Last;
      -- Die While Schleife läuft für jedes Zeichen der übergebenen Zeichenkette.
      WHILE Index /= 0 LOOP
         -- Das Erste und das Letzte Zeichen wird verglichen.
         IF Word(Index) /= Word(Word'Last - Index + 1) THEN
            RETURN False;
         END IF;
         
         Index := Index - 1;
      END LOOP;
      
      RETURN True;
   END Is_Palindrome_While;

END Palindromes;
