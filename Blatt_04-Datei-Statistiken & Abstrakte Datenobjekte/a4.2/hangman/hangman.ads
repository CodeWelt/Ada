--  FILE:    hangman.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 4
--  VERSION: $Revision: 123 $
--  DATE:    $Date: 2006-11-17 17:37:07 +0100 (Fri, 17 Nov 2006) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  PACKAGE Hangman
--
--  Bietet Funktionalitaet fuer das bekannte Galgenraten-Spiel:
--  1. Der Spielleiter waehlt ein Wort und setzt eine maximale Anzahl
--  erfolgloser Rateversuche fest.
--  2. Das System setzt alle Positionen des Worts auf den Status
--  "verdeckt" und das Spiel beginnt.
--
--  3. Das System gibt das Wort aus, allerdings wird an verdeckten
--  Positionen nicht der Buchstabe, sondern ein besonderes Zeichen
--  ausgegeben.
--  4. Der Spieler raet einen Buchstaben.
--  5. Das System deckt alle noch verdeckten Vorkommen dieses
--  Buchstabens in dem Wort auf. Wurde keine Position aufgedeckt, so
--  erhoeht das System die Anzahl der erfolglosen Rateversuche.
--  6. Sind alle Buchstaben aufgedeckt, so wird bei (8) fortgesetzt.
--  7. Hat der Spieler noch weitere Rateversuche uebrig (d.h. die
--  Anzahl der erfolglosen Rateversuche ist kleiner als die vom
--  Spielleiter festgesetzte maximale Anzahl), so wird bei (3)
--  fortgesetzt.
--
--  8. Hat der Spieler alle Buchstaben aufgedeckt so hat er gewonnen,
--  andernfalls verloren. Das Spiel ist beendet.
--
--
--  Ein Spiel wird gestartet mit der Prozedur Start_Game.
--
--  Mit der Funktion Is_Game_Active kann abgefragt werden, ob gerade
--  ein Spiel im Gange ist.
--
--  Das Spiel endet (ist nicht mehr aktiv) sobald alle Buchstaben
--  erraten wurden, sobald alle Rateversuche verbraucht sind oder wenn
--  Put_Solution aufgerufen wird.
--
--  Ein Buchstabe wird durch einen Aufruf der Prozedur Guess geraten.
--
--  Die Funktion Remaining_Guesses berechnet, wie viele Rateversuche
--  im aktiven Spiel verbleiben.
--
--  Die Funktion Remaining_Characters berechnet, wie viele Positionen
--  im Wort noch verdeckt sind.
--

package Hangman is


   --  EXCEPTION Game_Not_Active
   --
   --  Wird erhoben immer wenn eine Spielaktion ausgefuehrt werden
   --  soll, waehrend das Spiel nicht aktiv ist.
   Game_Not_Active : exception;


   --  EXCEPTION Illegal_Word
   --
   --  Wird erhoben falls ein Spiel mit einem Wort gestartet werden
   --  soll, das nicht zulaessig ist.
   Illegal_Word : exception;


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
      Guesses : in Positive);


   --  FUNCTION Is_Game_Active
   --
   --  Ermittelt ob gerade ein Spiel im Gange ist.
   --
   --  RETURNS: True, falls ein Spiel aktiv ist, False andernfalls.
   function Is_Game_Active
     return Boolean;


   --  FUNCTION Remaining_Guesses
   --
   --  Ermittelt die Anzahl der verbleibenden Rateversuche im
   --  aktuellen Spiel.
   --
   --  RETURNS: Anzahl der verbleibenden Rateversuche
   --  PRE: Is_Game_Active
   --  RAISES: Game_Not_Active - falls kein Spiel aktiv ist
   function Remaining_Guesses
     return Natural;


   --  FUNCTION Remaining_Characters
   --
   --  Ermittelt die Anzahl der noch nicht aufgedeckten
   --  Buchstabenpositionen. Diese Funktion darf auch nach beendetem
   --  Spiel aufgerufen werden um den Spielausgang zu erfragen.
   --
   --  RETURNS: Anzahl der verdeckten Buchstaben
   function Remaining_Characters
     return Natural;


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
     (Choice : in Character);


   --  PROCEDURE Put_Status
   --
   --  Gibt das aktuelle Spielwort aus. Noch nicht aufgedeckte
   --  Buchstabenpositionen werden durch ein '-' angezeigt. An
   --  aufgedeckten Positionen steht der entsprechende Buchstabe. Es
   --  wird nur das Spielwort und kein Zeilenumbruch ausgegeben.
   --
   --  Gibt nach Ende eines Spiels den Zustand am Ende des letzten
   --  Spiels aus.
   procedure Put_Status;


   --  PROCEDURE Put_Solution
   --
   --  Beendet das Spiel, falls eines aktiv war.  Gibt die Loesung des
   --  zuletzt aktiven Spiels aus. Nach dem Loesungswort wird kein
   --  Zeilenumbruch ausgegeben.
   --
   --  POST: Is_Game_Active = False
   procedure Put_Solution;


end Hangman;
