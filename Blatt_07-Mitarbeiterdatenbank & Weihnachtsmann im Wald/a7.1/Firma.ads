--  FILE:    Firma.ads
--  PROJECT: Programmieruebungen, Uebungsblatt 7
--  VERSION: 1.0
--  DATE:    17.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 7.1: Mitarbeiterdatenbank
--
--  Das Package bietet Funktionalität für eine
--  Mitarbeiter-Datenbank. Bitte entnehmen Sie weitere
--  Informationen aus den Kommentaren zu jeder Prozedur oder
--  Funktion.
--
-------------------------------------------------------------------
with Ada.Strings.Unbounded, Ada.Strings;
use  Ada.Strings.Unbounded, Ada.Strings;

package Firma is

   type Person is private;
   
   --  PROCEDURE Loadpersonnel
   --  Aus der Datei personnel.txt wird gelesen, jede Zeile
   --  durch die Prozedur Zerlege in ihre 3 Einzelteile zerlegt
   --  und in der Mitarbeiter-Datenbank gespeichert.
   procedure Loadpersonnel;
   
   --  PROCEDURE Zerlege
   --  Die Prozedur Zerlege wird von der Prozedur Loadpersonnel
   --  gebraucht um die gerade aus der Datei gelesene Zeile
   --  in ihre 3 Einzelteile (<key>#<name>#<boss>) zu zerlegen und
   --  in der Mitarbeiter-Datenbank abzulegen.
   --  PARAMETERS:
   --  Personalnummer, Name, Vorgesetzter sind die Werte der
   --  Person die belegt werden sollen.
   --  Source ist die gerade aus der Datei gelesene Zeile
   --  (<key>#<name>#<boss>) die zerlegt und abgelegt werden sollen.
   procedure Zerlege (Personalnummer : in out
   Integer; Name : in out Unbounded_String;
   Vorgesetzter : in out Integer; Source : in Unbounded_String);

   --  PROCEDURE SavepersonnelNeu
   --  Nachdem mit der Hire prozedur die eingelesene Mitarbeiter-Datenbank
   --  verändert wurde, wird hier eine Kopie erstellt und gespeichert.
   procedure SavepersonnelNeu;
   
   --  FUNCTION Find
   --  Die Prozedur gibt Name und Personalnummer des direkten
   --  Vorgesetzten zu einem bestimmten Mitarbeiter aus.
   --  
   --  PARAMETERS:
   --  FindPersonalnummer: FindPersonalnummer ist die
   --  Personalnummer als Integer Wert der gesucht wird.   
   --  RETURNS:
   --  Es wird der Eintrag aus der Mitarbeiter-Datenbank
   --  zurückgegeben bei der die Perstonalnummer
   --  übereinstimmt.
   function Find (FindPersonalnummer : in Integer) return Person;
   
   --  FUNCTION Find_String
   --  Die Prozedur gibt Name und Personalnummer des direkten
   --  Vorgesetzten zu einem bestimmten Mitarbeiter aus.
   --  
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  direkter Vorgesetzter gesucht wird.   
   --  RETURNS:
   --  Es wird der Eintrag aus der Mitarbeiter-Datenbank
   --  zurückgegeben bei der entweder Name oder Personalnummer
   --  übereinstimmt.   
   function Find_String (FindThis : in String) return Person;
   
   --  PROCEDURE List
   --  Die Prozedur gibt Name und Personalnummer aller Mitarbeiter
   --  in einer Liste aus.
   procedure List;
   
   --  PROCEDURE Supervisor
   --  Die Prozedur gibt Name und Personalnummer des direkten
   --  Vorgesetzten zu einem bestimmten Mitarbeiter aus.
   --  
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  direkter Vorgesetzter gesucht wird.
   procedure Supervisor (Name : in String);
   
   --  PROCEDURE Peers
   --  Die Prozedur gibt eine Liste der Namen und Personalnummern
   --  der Kollegen eines als String übergebenen Mitarbeiters aus.
   --  Mit Kollegen sind hier nur diejenigen Mitarbeiter gemeint,
   --  die den selben direkten Vorgesetzten haben.
   --
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  Kollegen gelistet werden sollen.
   procedure Peers (Name : in String);
   
   --  PROCEDURE Hire
   --  Berechnet für jedes Feld auf dem Schachbrett, wieviele Zuege
   --  das Pferdchen mindestens benoetigt, um von dem Feld 'Start' aus
   --  auf dieses Feld zu ziehen. Speichert diese Information in dem
   --  out-Parameter 'Board'.
   --
   --  PARAMETERS:
   --  + Start - Startfeld des Springers
   --  + Board - Spielfeld, Abbildung von Feld zu Anzahl Zuege 
   procedure Hire (P1 : in String; P2 : in String);
   
   --  PROCEDURE Team
   --  Die rekursive Prozedur gibt eine Liste der Namen und
   --  Personalnummern der Mitarbeiter mit (möglicherweise
   --  indirektem) Vorgesetztem aus.
   --
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  Vorgesetzter gefunden werden soll.
   procedure Team (Name : in String);

private
   --  TYPE Person
   --  Dieser Typ speichert für jede Person 3 Teile.
   --  Personalnummer und Vorgesetzter als Integer und Name
   --  als Unbounded_String.
   type Person is
      record
         Personalnummer : Integer := Integer'Last;
         Name : Ada.Strings.Unbounded.Unbounded_String
          := Null_Unbounded_String;
         Vorgesetzter : Integer := Integer'Last;
      end record;
      
end Firma;