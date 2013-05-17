--  FILE:    guthaben.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 11
--  VERSION: 1.0
--  DATE:    27.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
-- 
--  Aufgabe 11.1: Zinsrechnung
--
--  In dieser Aufgabe wird das momentane Guthaben auf einem
--  Sparkonto berechnet. Als Eingabe dienen zwei Textdateien,
--  einzahlung.txt und zinssatz.txt. In der Datei einzahlung.txt
--  werden Ein- und Auszahlungen auf das Konto mit jährlichen
--  Guthabenszinssatzes mit Datum der Änderung aufgelistet.
--  In der zinssatz.txt: Analog zu einzahlung.txt, jedoch wird
--  der Float-Wert als Zinssatz in Prozent pro Jahr interpretiert.
--
--  Dieses Programm dient zur Demonstration des Packages Zins.
--
-------------------------------------------------------------------
with Ada.Text_IO, Zins;
use  Ada.Text_IO, Zins;

procedure guthaben is
begin
   Put_Line ("Aufgabe 11.1: Zinsrechnung");
   --  Die Datei einzahlung.txt wird geladen.
   Load_einzahlung_txt;
   --  Die Datei zinssatz.txt wird geladen.
   Load_zinssatz_txt;
   --  Der Benutzer wird zur Eingabe eines
   --  Auflösungsdatums aufgefordert.
   Run;
   --  Der genützte Speicher beider Listen
   --  wird freigegeben.
   Destroy;

end guthaben;
