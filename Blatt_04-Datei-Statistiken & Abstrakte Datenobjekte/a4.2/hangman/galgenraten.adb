--  FILE:    galgenraten.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 4
--  VERSION: $Revision: 123 $
--  DATE:    $Date: 2006-11-17 17:37:07 +0100 (Fri, 17 Nov 2006) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  PROGRAM Galgenraten-Spiel
--
--  Das bekannte Galgenraten-Spiel. Verwendet Paket Hangman, siehe
--  dort fuer Beschreibung.
--


with Ada.Text_IO;
with Hangman;


procedure Galgenraten
is
   Word    : constant String := "Programmieruebungen";
   Guesses : constant := 10;

   --  PROCEDURE Put_Status
   --
   --  Gibt dem Benutzer das teilweise aufgedeckte Wort aus.
   procedure Put_Status
   is
   begin
      Ada.Text_IO.Put ("::: ");
      Hangman.Put_Status;
      Ada.Text_IO.Put_Line (" :::");
   end Put_Status;

   --  Aufzaehlungstyp fuer Ja/Nein-Fragen
   type Continue_Options is (Ja, J, Undecided, Nein, N);
   Play_Again : Continue_Options;
   package Continue_IO is new Ada.Text_IO.Enumeration_IO
     (Enum => Continue_Options);

   Choice : Character;

   use Ada.Text_IO;
begin
   Game_Loop : loop
      --  Beginn eines neuen Spiels
      Hangman.Start_Game (Word, Guesses);

      --  Benutzer kann wiederholt Rateversuche abgeben, bis Spiel
      --  beendet
      while Hangman.Is_Game_Active loop
         Put_Line
           ("Noch" & Integer'Image (Hangman.Remaining_Guesses) &
            " Versuche fuer" & Integer'Image (Hangman.Remaining_Characters) &
            " Buchstaben!");
         Put_Status;
         loop
            Put ("Naechster Buchstabe: ");
            Get (Choice);
            exit when Choice in 'a' .. 'z' or Choice in 'A' .. 'Z';
            Put ("Wie bitte? ");
         end loop;

         Hangman.Guess (Choice);
      end loop;

      --  Gewonnen/Verloren?
      if Hangman.Remaining_Characters = 0 then
         Put_Line ("Herzlichen Glueckwunsch! Du hast gewonnen!");
      else
         Put_Line ("ICH habe gewonnen!");
         Put_Status;
      end if;
      Put ("=== ");
      Hangman.Put_Solution;
      Put_Line (" ===");

      --  Abfrage, ob nochmal spielen
      New_Line;
      Play_Again_Question : loop
         Put ("Nochmal? ");
         begin
            Continue_IO.Get (Play_Again);
         exception
            when Ada.Text_IO.Data_Error =>
               --  Fehlerhafte Eingabe
               Put ("Wie bitte? ");
               Play_Again := Undecided;
         end;
         case Play_Again is
            when Ja | J =>
               exit Play_Again_Question;
            when Nein | N =>
               exit Game_Loop;
            when Undecided =>
               null;
         end case;
      end loop Play_Again_Question;
   end loop Game_Loop;

   Put_Line ("Bis zu naechsten Mal!");
exception
   when Hangman.Illegal_Word =>
      Put_Line ("Das Wort """ & Word & """ wird nicht akzeptiert!");
end Galgenraten;
