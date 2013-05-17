--  FILE:    Put_Adjacency.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 11
--  VERSION: 1.0
--  DATE:    27.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
-- 
--  Aufgabe 11.2: Externe Repr�sentation
--
--  In dieser Aufgabe wird ein gerichteter Graph aus der Datei
--  graphen.txt geladen und mit Hilfe von Adjazenzlisten
--  repr�sentiert. Aus dieser Darstellung wird eine
--  Adjazenzmatrix des Graphen erstellt.
--  Die graphen.txt ist wie folgt aufgebaut: In jeder Zeile
--  der Datei ist eine Kante spezifiziert mit Startknoten-Id,
--  Kantengewicht und Zielknoten-Id.
--  Eine Zeile hat folgendes Format, wobei Source f�r die
--  Knoten-Id des Quellknotens steht, Target f�r die Knoten-Id
--  des Zielknotens und W f�r das Gewicht der Kante:
--  Source (W) Target
--  Das Programm Put_Adjacency.adb demonstriert die Funktionalit�t
--  das Packages graphen.
--
-------------------------------------------------------------------
with graphen, Ada.Text_IO;
use  graphen, Ada.Text_IO;

procedure Put_Adjacency is
begin
   Put_Line ("Aufgabe 11.2: Externe Repr�sentation");
   New_Line;
   --  Die Datei graphen.txt wird geladen.
   Load;
   --  Die Adjazenzmatrix wird formatiert ausgegeben.
   Ausgabe;

end Put_Adjacency;
