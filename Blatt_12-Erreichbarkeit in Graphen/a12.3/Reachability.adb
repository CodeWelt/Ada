--  FILE:    Reachability.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 12
--  VERSION: 1.0
--  DATE:    04.02.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
-- 
--  Aufgabe 12.3: Erreichbarkeit in Graphen
--
--  In dieser Aufgabe wird ein gerichteter Graph aus der Datei
--  graph.txt geladen und mit Hilfe von Adjazenzlisten
--  repr�sentiert. Aus dieser Darstellung wird eine
--  Adjazenzmatrix des Graphen erstellt.
--  Die graph.txt ist wie folgt aufgebaut: In jeder Zeile
--  der Datei ist eine Kante spezifiziert mit Startknoten-Id,
--  Kantengewicht und Zielknoten-Id.
--  Eine Zeile hat folgendes Format, wobei Source f�r die
--  Knoten-Id des Quellknotens steht, Target f�r die Knoten-Id
--  des Zielknotens und W f�r das Gewicht der Kante:
--  Source (W) Target
--  Das Programm Reachability.adb demonstriert die Funktionalit�t
--  das Packages graphen.
--  Der Benutzer wird zur Eingabe einer Knoten-Id aufgefordert.
--  Diese Knoten-Id identifiziert einen Start-Knoten.
--  Von diesem Start-Knoten aus werden alle Knoten aufgelistet,
--  die davon erreichbar sind.
--
-------------------------------------------------------------------
with graphen, Ada.Text_IO;
use  graphen, Ada.Text_IO;

procedure Reachability is
begin
   Put_Line ("Aufgabe 12.3: Erreichbarkeit in Graphen");
   New_Line;
   --  Die Datei graph.txt wird geladen.
   Load;
   --  Der Benutzer wird zur Eingabe einer Knoten-Id aufgefordert.
   --  Diese Knoten-Id identifiziert einen Start-Knoten.
   --  Von diesem Start-Knoten aus werden alle Knoten aufgelistet,
   --  die davon erreichbar sind.
   Reachable;

end Reachability;
