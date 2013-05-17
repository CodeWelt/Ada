--  FILE:    graphen.ads
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
--  repräsentiert. Aus dieser Darstellung wird eine
--  Adjazenzmatrix des Graphen erstellt.
--  Die graph.txt ist wie folgt aufgebaut: In jeder Zeile
--  der Datei ist eine Kante spezifiziert mit Startknoten-Id,
--  Kantengewicht und Zielknoten-Id.
--  Eine Zeile hat folgendes Format, wobei Source für die
--  Knoten-Id des Quellknotens steht, Target für die Knoten-Id
--  des Zielknotens und W für das Gewicht der Kante:
--  Source (W) Target
--
-------------------------------------------------------------------
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package graphen is

   type Kantenliste is private;
   type Knotenliste is private;


   --  PROCEDURE Reachable
   --
   --  Der Benutzer wird zur Eingabe einer Knoten-Id aufgefordert.
   --  Diese Knoten-Id identifiziert einen Start-Knoten.
   --  Von diesem Start-Knoten aus werden alle Knoten aufgelistet,
   --  die davon erreichbar sind.
   procedure Reachable;
     

   --  PROCEDURE Load
   --
   --  In dieser Prozedur wird die Datei graph.txt gelesen
   --  und die daraus resultierenden Listen ermittelt.
   procedure Load;
   

   --  PROCEDURE Ausgabe
   --
   --  In dieser Prozedur wird eine Matrix in den Dimensionen,
   --  bestimmt durch die Anzahl der Elemente in der Knotenliste,
   --  definiert, mit den Kantengewichten gesetzt und formatiert
   --  ausgegeben. Es werden alle Felder der Matrix für den Anfang
   --  auf null gesetzt.
   procedure Ausgabe;


   type SourceWTarget is
      record
         Source : Unbounded_String := Null_Unbounded_String;
         --  Kantengewichte sind natürliche Zahlen im
         --  Bereich 1-9.
         W : Natural := 0;
         Target : Unbounded_String := Null_Unbounded_String;
      end record;

private

   type Knoten;
   type Kante;

   type Kantenliste is access Kante;
   type Knotenliste is access Knoten;


   --  TYPE Knoten
   --
   --  Der Knoten ist ein Element der
   --  Knotenliste. Jeder Knoten hat eine
   --  Kantenliste bestehend aus Elementen vom
   --  typ Kante.
   type Knoten is
      record
         Next   : Knotenliste;
         Kanten : Kantenliste;
         Id     : Unbounded_String := Null_Unbounded_String;
      end record;


   --  TYPE Kante
   --
   --  Die Kante ist ein Element der
   --  Kantenliste. Jede Kante hat ein
   --  Kantenziel welches ein Zeiger auf
   --  ein Knoten in der Knotenliste ist.
   type Kante is
      record
         Next : Kantenliste;
         --  Zeiger auf Knoten.
         Kantenziel : Knotenliste;
         --  Kantengewichte sind natürliche Zahlen im
         --  Bereich 1-9.
         Gewicht : Natural;
      end record;

   subtype Graph is Knotenliste;

end graphen;
