--  FILE:    hangman.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 4
--  VERSION: 1.0
--  DATE:    24.11.2006
--  AUTHOR: http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 4.2: Abstrakte Datenobjekte
--
--  Der Spielleiter wählt ein Wort und setzt eine maximale Anzahl
--  erfolgloser Rateversuche fest.
--  Es werden alle Positionen des Worts auf den Status "verdeckt"
--  gesetzt und das Spiel beginnt.
--
--  Beim starten des Spiels wird in der PROCEDURE Start_Game erst
--  geprüft ob das zu eratende Wort aus den Zeichen 'a' bis 'z' und
--  'A' bis 'Z' besteht. Im folgenden wird der Unbounded_String
--  Guessed, der die selbe Länge hat wie das zu eratende Wort,
--  erstellt. Guessed enthält parallel zum gesuchten Wort für
--  jeden Buchstaben entweder eine 0 oder 1 als "verdeckt"
--  oder nicht.
--  Mit der PROCEDURE Guess wird ein Buchstabe geraten.
--  Der geratene Buchstabe wird mit jedem Buchstaben des gesuchten
--  Worts verglichen. Wenn eine Übereinstimmung gefunden wurde,
--  wird an der selben Index-Position im Unbounded_String Guessed
--  eine 1 geschrieben als "aufgedeckt".
--    Beispiel: Programmieruebungen
--   Status ist -------------------
--  Guessed ist 0000000000000000000
--  Werden jetzt die Buchstaben p, R, m und e geraten:
--   Status ist Pr--r-mm-er-e----e-
--  Guessed ist 1100101101101000010
--  Auf diese Weise wird auch in PROCEDURE Guess überprüft ob ein
--  Buchstabe bereits geraten wurde oder nicht.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings, Ada.Characters.Handling;
use  Ada.Text_IO, Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings, Ada.Characters.Handling;

package body Hangman is


   ------------------
   -- Spielzustand --
   ------------------


   --  GLOBAL VARIABLE Search_Word
   --
   --  Speichert das zu erratende Wort.
   Search_Word : Ada.Strings.Unbounded.Unbounded_String
      := Null_Unbounded_String;

   --  GLOBAL VARIABLE Guessed
   --
   --  Markiert, welche Buchstabenpositionen bereits aufgedeckt
   --  wurden.
   --
   --  TODO: beschreiben Sie welchen Inhalt die Variable in jedem
   --  Spielzustand genau hat.
   Guessed   : Ada.Strings.Unbounded.Unbounded_String := Null_Unbounded_String;

   --  GLOBAL VARIABLE Guess_Total
   --
   --  Speichert die maximale Anzahl erfolgloser Rateversuche.
   Guess_Total : Natural := 0;

   --  GLOBAL VARIABLE Guess_Count
   --
   --  Speichert die Anzahl im aktiven Spiel abgegebener Rateversuche
   Guess_Count : Natural := 0;

   --  GLOBAL VARIABLE Game_Active
   --
   --  Speichert ob gerade ein Spiel aktiv ist, oder nicht.
   Game_Active : Boolean := False;



   -----------------
   -- Spielbeginn --
   -----------------

   --  PROCEDURE Start_Game
   --
   --  Beginnt ein neues Galgenraten-Spiel. Ist bereits ein Spiel
   --  aktiv, so wird dieses Spiel abgebrochen und das neue begonnen.
   --
   --  Das zu erratende Wort muss aus den Buchstaben 'a'..'z' und
   --  'A'..'Z' bestehen. Ist dies nicht der Fall, so wird kein neues
   --  Spiel begonnen, sondern die Ausnahme Illegal_Word erhoben. War
   --  zuvor ein Spiel aktiv, so wird dieses trotzdem abgebrochen.
   --
   --  PARAMETERS:
   --  + Word: das zu erratende Wort, alle Buchstaben muessen in den
   --  Bereichen 'a'..'z' oder 'A'..'Z' liegen; andernfalls wird
   --  Illegal_Word erhoben
   --  + Guesses: Anzahl der erlaubten erfolglosen Rateversuche, bis
   --  das Spiel verloren ist
   --  RAISES: Illegal_Word - falls der Parameter Word ein nicht
   --                         erlaubtes Zeichen enthaelt
   --  POST: Is_Game_Active and
   --        (Remaining_Guesses = Guesses) and
   --        (Remaining_Characters = Word'Length)
   procedure Start_Game
     (Word    : in String;
      Guesses : in Positive)
   is
      Temp : Character := 'a';
   begin
      Search_Word := Null_Unbounded_String;
      Guessed := Null_Unbounded_String;
      Game_Active := True;
      Guess_Total := Guesses;
      Guess_Count := Guesses;
      Search_Word := Ada.Strings.Unbounded.To_Unbounded_String (Word);
      
      --  Die Schleife läuft für jedes Zeichen im gesuchten Wort.
      for Laufvar in 1 .. Length (Search_Word) loop
         --  Das aktuelle Zeichen wird eingelesen.
         Temp := Element (Search_Word, Laufvar);
         --  Wenn dieses Zeichen nicht in a-z oder A-Z ist,
         --  wird die Ausnahme Illegal_Word erhoben.
         if Temp in 'a' .. 'z' or Temp in 'A' .. 'Z' then
            null;
         else
            --  War zuvor ein Spiel aktiv, so wird dieses
            --  trotzdem abgebrochen.
            Game_Active := False;
            raise Illegal_Word;
         end if;
      end loop;
      
      for Laufvar in 1 .. Length (Search_Word) loop
         Guessed := Guessed & "0";  
      end loop;

   end Start_Game;


   -----------------
   -- Inspektoren --
   -----------------

   --  FUNCTION Is_Game_Active
   --
   --  Ermittelt ob gerade ein Spiel im Gange ist.
   --
   --  RETURNS: True, falls ein Spiel aktiv ist, False andernfalls.
   function Is_Game_Active
     return Boolean
   is
   begin
      return Game_Active;
   end Is_Game_Active;

   --  FUNCTION Remaining_Guesses
   --
   --  Ermittelt die Anzahl der verbleibenden Rateversuche im
   --  aktuellen Spiel.
   --
   --  RETURNS: Anzahl der verbleibenden Rateversuche
   --  PRE: Is_Game_Active
   --  RAISES: Game_Not_Active - falls kein Spiel aktiv ist
   function Remaining_Guesses
     return Natural
   is
   begin      
      if Is_Game_Active = False then
         raise Game_Not_Active;
      end if;
      return Guess_Count;
   end Remaining_Guesses;

   --  FUNCTION Remaining_Characters
   --
   --  Ermittelt die Anzahl der noch nicht aufgedeckten
   --  Buchstabenpositionen. Diese Funktion darf auch nach beendetem
   --  Spiel aufgerufen werden um den Spielausgang zu erfragen.
   --
   --  RETURNS: Anzahl der verdeckten Buchstaben
   function Remaining_Characters
     return Natural
   is
      Counter : Natural := 0;
   begin
      for Laufvar in 1 .. Length (Guessed) loop
         if Element (Guessed, Laufvar) = '0' then
            Counter := Counter + 1;
         end if;
      end loop;
            
      return Counter;
   end Remaining_Characters;



   --------------
   -- Aktionen --
   --------------

   --  PROCEDURE Guess
   --
   --  Fuehrt einen Rateversuch durch und aktualisiert den
   --  Spielstatus. Falls der Rateversuch keinen neuen Buchstaben des
   --  Worts aufdecken konnte, so verringert sich die Anzahl der
   --  verbleibenden Rateversuche um 1. Bereits aufgedeckte Buchstaben
   --  werden nicht erneut aufgedeckt, sondern zählen als erfolgloser
   --  Rateversuch.
   --
   --  Das Spiel wird von dieser Prozedur beendet wenn alle
   --  Rateversuche verbraucht sind oder wenn das Wort komplett
   --  erraten wurde.
   --
   --  Es soll nicht zwischen Gross- und Kleinschreibung unterschieden
   --  werden, d.h. der Buchstabe 'a' deckt alle Positionen des Worts
   --  auf, an denen 'a' oder 'A' steht.
   --
   --  PARAMETERS:
   --  + Choice: der geratene Buchstabe
   --  PRE: Is_Game_Active
   --  RAISES: Game_Not_Active - falls kein Spiel aktiv ist
   procedure Guess
     (Choice : in Character)
   is
      Found : Boolean := False;
   begin

      if Is_Game_Active = False then
         raise Game_Not_Active;
      end if;      
      --  Die Schleife läuft für jedes Zeichen im gesuchten Wort.
      for Laufvar in 1 .. Length (Search_Word) loop
         --  Zuerst werden beide Character die verglichen werden sollen
         --  zu Grossbuchstaben gemacht um keinen Unterschied zwischen
         --  Gross- und Kleinbuchstaben zu machen.
         --  Jedes Zeichen im gesuchten Wort wird mit dem geratenen
         --  Character verglichen.
         if To_Upper (Element (Search_Word, Laufvar)) = To_Upper (Choice) then
            --  Bereits aufgedeckte Buchstaben werden nicht erneut
            --  aufgedeckt, sondern zählen als erfolgloser Rateversuch.
            if Element (Guessed, Laufvar) = '1' then
               Found := False;
            else
               --  Wurde der Buchstabe noch nicht aufgedeckt so
               --  wird dies durch eine Eins im Unbounded_String
               --  Guessed vermerkt.
               Replace_Element (Guessed, Laufvar, '1');
               Found := True;
            end if;   
            
         end if;
      
      end loop;
      
      --  Falls der Ratebersuch keinen neuen Buchstaben des
      --  Worts aufdecken konnte, so verringert sich die Anzahl
      --  der verbleibenden Rateversuche um 1.
      if Found = False then
         Guess_Count := Guess_Count - 1;
      end if;
      
      --  Das Spiel wird beendet wenn alle Rateversuche verbraucht
      --  sind oder wenn das Wort komplett erraten wurde.
      if Guess_Count = 0 or Hangman.Remaining_Characters = 0 then
         Game_Active := False;
      end if;
      
   end Guess;

   --  PROCEDURE Put_Status
   --
   --  Gibt das aktuelle Spielwort aus. Noch nicht aufgedeckte
   --  Buchstabenpositionen werden durch ein '-' angezeigt. An
   --  aufgedeckten Positionen steht der entsprechende Buchstabe. Es
   --  wird nur das Spielwort und kein Zeilenumbruch ausgegeben.
   --
   --  Gibt nach Ende eines Spiels den Zustand am Ende des letzten
   --  Spiels aus.
   procedure Put_Status
   is
   begin
      for Laufvar in 1 .. Length (Guessed) loop
         if Element (Guessed, Laufvar) = '0' then
            Put ("-");
         else
            Put (Element (Search_Word, Laufvar));
         end if;
      end loop;
      
   end Put_Status;

   --  PROCEDURE Put_Solution
   --
   --  Beendet das Spiel, falls eines aktiv war.  Gibt die Loesung des
   --  zuletzt aktiven Spiels aus. Nach dem Loesungswort wird kein
   --  Zeilenumbruch ausgegeben.
   --
   --  POST: Is_Game_Active = False
   procedure Put_Solution
   is
   begin
      Put (Search_Word);
      Game_Active := False;
   end Put_Solution;


end Hangman;
