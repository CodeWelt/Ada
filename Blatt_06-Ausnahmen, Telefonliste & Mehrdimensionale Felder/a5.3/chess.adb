--  FILE:    chess.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 6
--  VERSION: 1.0
--  DATE:    09.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 6.3: Mehrdimensionale Felder
--  Das Package bietet Funktionalitaet um die Anzahl benoetigter
--  Spielzuege eines Pferdchens zu allen Feldern auf einem
--  Schachbrett zu berechnen.
--
-------------------------------------------------------------------

package body Chess is

   
   procedure Rechne
   (Board : in out Chess_Board; Feld : in Board_Position; Zug : in Natural);

   --  PROCEDURE Knight_Distance
   --  Berechnet für jedes Feld auf dem Schachbrett, wieviele Zuege
   --  das Pferdchen mindestens benoetigt, um von dem Feld 'Start' aus
   --  auf dieses Feld zu ziehen. Speichert diese Information in dem
   --  out-Parameter 'Board'.
   --
   --  PARAMETERS:
   --  + Start - Startfeld des Springers
   --  + Board - Spielfeld, Abbildung von Feld zu Anzahl Zuege
   procedure Knight_Distance
     (Start : in     Board_Position;
      Board :    out Chess_Board) is
   begin
      --  Alle Felder des Chess_Board werden für den Anfang auf eine hohe Zahl
      --  wie zum Beispiel 10 gesetzt, um nacher die kleinste Zahl zu
      --  ermitteln.
      Board := (others => (others => 10));
      --  Das Feld mit dem angefangen wird soll Null enthalten.
      Board (Start.H, Start.V) := 0;
      --  Die rekursive Prozedur Rechne wird mit dem Startfeld das gerade mit
      --  Null belegt wurde und als Zug-Zahl 1 für den ersten Zug gestartet.
      Rechne (Board, Start, 1);
   end Knight_Distance;

   --  PROCEDURE Rechne
   --  Die Prozedur Rechne wird von der Prozedur Knight_Distance aufgerufen
   --  um rekursiv alle Felder des Schachbretts mit werten zu füllen.
   --
   --  PARAMETERS:
   --  Board: Dies ist das Schachbrett welches berechnet werden soll
   --  welches zuvor auf allen Feldern 10 gesetzt wurde und das Startfeld
   --  auf 0.
   --  Feld: Dies ist das aktuelle Feld auf dem sich das Pferdchen befindet und
   --  ziehen soll.
   --  Zug: Zug ist die Anzahl der züge bis zum aktuellen Feld und dem Zug der
   --  gemacht werden soll.
   procedure Rechne
   (Board : in out Chess_Board; Feld : in Board_Position; Zug : in Natural) is
      Param : Board_Position;
   begin
      for I in -2 .. 2 loop
         for J in -2 .. 2 loop
            --  Wenn das aktuelle Feld eine mögliche Bewegung vom Pferdchen ist
            --  wird fortgefahren. Das bedeutet die summe der Absolutwerte ist
            --  3 (Beispiel abs (-1) + abs (-2) = 3).
            if ((abs (I) + abs (J) = 3) and
               --  Hier wird der aktuelle horizontal Wert mit der aktuellen
               --  horizontalen addiert nachdem
               --  der Buchstabe in einen ASCII Zahlencode umgewandelt wurde.
               --  Daraufhin wird der Wert
               --  wieder zum neuen Buchstaben gewandelt und überprüft ob er
               --  im zulässigen Bereich liegt.
               --  Wenn das der fall ist wird fortgefahren.
               Character'Val ((Character'Pos (Feld.H)) + J) in Horizontal'Range
               and
               --  Wie oben wird hier die vertikale mit der aktuellen
               --  vertikalen addiert und überprüft ob das Ergebnis im
               --  zulässigen Bereich liegt. Wenn das der Fall ist wird
               --  fortgefahren.
               Feld.V + I in Vertical'Range) and then
               --  Wenn sowohl die aktuelle vertikale als auch die horizontale
               --  legitim sind wird hier überprüft ob das eventuell zuvor
               --  berechnete Feld mit einer größeren Zug-Zahl belegt wurde
               --  als der aktuelle Zug.
               Board (Character'Val
               ((Character'Pos (Feld.H)) + J), Feld.V + I) > Zug then
               --  Wenn die zuvor belegte Zug-Zahl größer als der aktuelle Zug
               --  war wird hier das Feld mit der neuen Zug-Zahl des aktuellen
               --  Zugs belegt.
               Board (Character'Val
               ((Character'Pos (Feld.H)) + J), Feld.V + I) := Zug;
               --  Der Parameter der die aktuelle Position representiert wird
               --  das aktuelle Feld zugeordnet um diesen Parameter an den
               --  rekursiven Aufruf zu übergeben.
               Param.H := Character'Val ((Character'Pos (Feld.H)) + J);
               Param.V := Feld.V + I;
               --  Die Prozedur Rechne wird hier rekursiv aufgerufen.
               --  Das Chess_Board, die vorbereitete neue Position und die
               --  Anzahl der Züge bisher + 1 werden übergeben.
               Rechne (Board, Param, Zug + 1);
            end if;
         end loop;
      end loop;
      
   end Rechne;
end Chess;