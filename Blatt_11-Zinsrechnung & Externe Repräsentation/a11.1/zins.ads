--  FILE:    zins.ads
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
--  Das Package zins bietet Funktionalität rund um Zinsrechnung.
--
-------------------------------------------------------------------
with Dates;
use  Dates;

package Zins is

   type Zeiger is private;

   --  PROCEDURE Load_einzahlung_txt
   --
   --  In der Prozedur Load_einzahlung_txt wird die Datei
   --  Zeile für Zeile eingelesen, zerlegt und die Bestandteile
   --  der Zeile in einen neuen Eintrag in der Liste ZahlungAnchor
   --  für die weitere Verwendung gespeichert.   
   procedure Load_einzahlung_txt;


   --  PROCEDURE Load_zinssatz_txt
   --
   --  In der Prozedur Load_zinssatz_txt wird die Datei
   --  Zeile für Zeile eingelesen, zerlegt und die Bestandteile
   --  der Zeile in einen neuen Eintrag in der Liste ZinsAnchor
   --  für die weitere Verwendung gespeichert.
   procedure Load_zinssatz_txt;
   

   --  PROCEDURE Run
   --
   --  Die Prozedur setzt das Startdatum auf den ersten
   --  Eintrag in der ZahlungAnchor Liste und führt solange
   --  Schritte aus bis das vom Benutzer eingegebene Datum
   --  erreicht ist.
   procedure Run;


   --  PROCEDURE Destroy
   --
   --  Es wird der Speicher beider Listen, ZahlungAnchor
   --  und ZinsAnchor, freigegeben und am ende die
   --  globalen Zeiger auf null gesetzt.
   procedure Destroy;


   --  TYPE Date_Float
   --
   --  Dieser Typ wird dazu gebraucht um jede Zeile
   --  die aus der Datei gelesen wird in ihre zwei
   --  Einzelteile Datum und Float-Wert zu zerlegen.
   type Date_Float is
   record
      Datum : Date;
      Zahl : Float := 0.00;
   end record;

private

   type Eintrag;
   --  Zeiger auf einen Eintrag der Liste.
   type Zeiger is access Eintrag;
   --  Eintrag definiert den Inhalt eines
   --  Eintrags der Liste.
   type Eintrag is record
      Datum : Date;
      Zahl : Float := 0.00;
      Next : Zeiger := null;
   end record;

end Zins;