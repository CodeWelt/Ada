--  FILE:    PManager.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 7
--  VERSION: 1.0
--  DATE:    17.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 7.1: Mitarbeiterdatenbank
--
--  PManager wird durch Kommandozeilen-Argumente gesteuert.
--  Als Personaldatenbank wird immer die Datei personnel.txt
--  verwendet. Jeder Aufruf hat das folgende Format:
--  PManager command [arg [arg2]]
--
-------------------------------------------------------------------
with Ada.Text_IO, Firma, Ada.Command_Line;
use  Ada.Text_IO, Firma, Ada.Command_Line;

procedure PManager is
   Hilfe : exception;
begin
   Put_Line ("Aufgabe 7.1: Mitarbeiterdatenbank");
   --  Die personnel.txt wird geladen.
   Loadpersonnel;
   --  Je nach Kommandozeilen-Argument wird
   --  die dazugehörige Prozedur mit Parametern 
   --  im Package Firma aufgerufen.
   if Argument_Count = 0 then
      raise Hilfe;
   elsif Argument (1) = "list" then
      List;
   elsif Argument (1) = "supervisor" then
      Supervisor (Argument (2));
   elsif Argument (1) = "peers" then 
      Peers (Argument (2));
   elsif Argument (1) = "team" then
      Team (Argument (2));
   elsif Argument (1) = "hire" then
      Hire (Argument (2), Argument (3));
   else
      raise Hilfe;
   end if;

exception
   when Hilfe =>
      Put_Line ("PManager wird durch Kommandozeilen-Argumente gesteuert.");
      Put_Line ("Als Personaldatenbank wird immer die Datei personnel.txt");
      Put_Line ("verwendet.");
      New_Line;
      Put_Line ("Jeder Aufruf hat das folgende Format:");
      Put_Line ("PManager command [arg [arg2]]");
      Put_Line ("Es folgen die möglichen commands:");
      Put_Line ("list:       gibt eine Liste aller Mitarbeiter mit Ihren");
      Put_Line ("            Peronalnummern aus.");
      Put_Line
       ("supervisor: gibt Name und Personalnummer des direkten Vorgesetzen");
      Put_Line ("            zu einem bestimmten Mitarbeiter aus. arg muss");
      Put_Line ("            angegeben werden und muss ein Name oder eine");
      Put_Line ("            Personalnummer sein.");
      Put_Line
       ("peers:      Gibt eine Liste der Namen und Personalnummern der");
      Put_Line ("            Kollegen eines Mitarbeiters arg aus.");
      Put_Line
       ("team:       Gibt eine Liste aus Namen und Personalnummern der");
      Put_Line ("            Mitarbeiter mit (möglicherweise indirektem)");
      Put_Line ("            Vorgesetztem arg aus.");
      Put_Line ("hire:       fügt der Mitarbeiter-Datenbank einen neuen");
      Put_Line
       ("            Mitarbeiter hinzu. arg identifiziert den direkten");
      Put_Line
       ("            Vorgesetzten des neuen Mitarbeiters und arg2 ist der");
      Put_Line ("            Name des neuen Mitarbeiters.");

end PManager;