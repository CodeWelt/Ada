--  FILE:    chess.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 6
--  VERSION: $Revision: 183 $
--  DATE:    $Date: 2006-12-01 13:23:29 +0100 (Fri, 01 Dec 2006) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  Bietet Funktionalitaet um die Anzahl benoetigter Spielzuege eines
--  Springers zu allen Feldern auf einem Schachbrett zu berechnen.
--

package Chess is

   --  SUBTYPE Horizontal
   --
   --  Die Spalten eines Schachbretts werden durch die Buchstaben 'A'
   --  .. 'H' benannt.
   subtype Horizontal is Character range 'A' .. 'H';

   --  SUBTYPE Vertical
   --
   --  Die Zeilen eines Schachbretts werden durch die Zahlen 1 .. 8
   --  nummeriert.
   subtype Vertical is Integer range 1 .. 8;

   --  TYPE Chess_Board
   --
   --  Modelliert eine Abbildung von Schachbrett-Feld (angegeben durch
   --  die Indizes Zeile, Spalte) zu Anzahl Zuege, die ein Springer
   --  benoetigt um das Feld zu erreichen.
   type Chess_Board is array (Horizontal, Vertical) of Natural;

   --  TYPE Board_Position
   --
   --  Repräsentiert eine Identifikation eines bestimmten Felds auf
   --  einem Schachbrett.
   type Board_Position is
      record
         --  Spalte auf dem Schachbrett
         H : Horizontal;
         --  Zeile auf dem Schachbrett
         V : Vertical;
      end record;

   --  PROCEDURE Knight_Distance
   --
   --  Berechnet für jedes Feld auf dem Schachbrett, wieviele Zuege
   --  ein Springer mindestens benoetigt, um von dem Feld 'Start' aus
   --  auf dieses Feld zu ziehen. Speichert diese Information in dem
   --  out-Parameter 'Board'.
   --
   --  PARAMETERS:
   --  + Start - Startfeld des Springers
   --  + Board - Spielfeld, Abbildung von Feld zu Anzahl Zuege
   procedure Knight_Distance
     (Start : in     Board_Position;
      Board :    out Chess_Board);

end Chess;
