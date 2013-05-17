--  FILE:    Weihnachtsmann.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 7
--  VERSION: 1.0
--  DATE:    15.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 7.2: Weihnachtsmann im Wald
--
--  Auf seinen Reisen verirrt sich der Wihnachtsmann im Wald.
--  Er schaut sich um und stellt überrascht fest, dass alle
--  Bäume völlig regelmäßig auf einem quadratischen Raster
--  stehen und zudem einen punktförmigen Querschnitt haben.
--  Von seinem Standpunkt aus kann er allerdings nicht alle
--  Bäume sehen, da weiter entfernte manchmal durch nähere
--  verdeckt werden.
--  Das Programm zeichnet wie in der Skizze für einen
--  vom Benutzer bestimmten Bereich alle für den
--  Weihnachtsmann sichtbaren Bäume.
--  Der Weihnachtsmann steht immer im Ursprung
--  des Koordinatensystems.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO;

procedure Weihnachtsmann is
   Bereich : Integer := 99;
   Eingabeungueltig : exception;
   
   --  FUNCTION GCD
   --
   --  Berechnet den groessten gemeinsamen Teiler
   --  zweier natuerlicher Zahlen.
   --
   --  RETURNS: ggT von First und Second
   function GCD
     (First  : in Integer;
      Second : in Integer)
     return Integer
   is
      A : Integer := Integer'Max (First, Second);
      B : Integer := Integer'Min (First, Second);
      H : Integer;
   begin
      while B /= 0 loop
         H := A mod B;
         A := B;
         B := H;
      end loop;
      return A;
   end GCD;

begin

   Put_Line ("Aufgabe 7.2: Weihnachtsmann im Wald");
   Put ("In welchem Bereich soll ausgegeben werden?: ");
   Get (Bereich);
   if Bereich < 0 then
      raise Eingabeungueltig;
   end if;

   --  Der Wald wird mit hilfe des declare Blocks im vom Benutzer
   --  angegebenen Bereich deklariert und alle Felder werden für den
   --  Anfang auf True gesetzt.   
   declarethis :
   declare
      Wald : array (Bereich * (-1) .. Bereich, Bereich * (-1) .. Bereich) of 
      Boolean := (others => (others => True));
   begin

      --  Es folgt die Formatierte Ausgabe der x-Koordinaten.
      --  Um die Ausgabe mit der cmd.exe formatiert auszugeben
      --  sollte maximal ein Bereich von 12 angegeben werden.
      --  Es ist jedoch mit der folgenden Formatierung ein
      --  Bereich bis zu 99 möglich.
      Put ("   ");
      for Laufvar in Wald'Range loop
         if abs (Laufvar) > 9 then
            Put (Laufvar'Img);
         else
            Put (Laufvar'Img & " ");
         end if;
      end loop;
      New_Line;

      --  Die Schleife läuft für den vertikalen
      --  Bereich des Walds.
      for Laufvarx in Wald'Range loop
         --  Es folgt die formatierte Ausgabe der aktuellen
         --  vertikalen Koordinate.
         if abs (Laufvarx) > 9 then
            Put (Laufvarx'Img);
         else
            Put (" " & Laufvarx'Img);
         end if;
                  
         --  Die Schleife läuft für den horizontalen
         --  Bereich des Walds.
         for Laufvary in Wald'Range loop
            --  Wenn der ggT der Absolutwerte der beiden aktuellen
            --  Koordinaten nicht 1 ist, wird das Feld
            --  auf False gesetzt. Das bedeutet der Baum ist
            --  für den Weihnachtsmann nicht sichtbar.
            if GCD (abs (Laufvarx), abs (Laufvary)) /= 1 then
               Wald (Laufvarx, Laufvary) := False;
            end if;
            
            if Laufvarx = 0 and Laufvary = 0 then
               Put (" W ");
            elsif Wald (Laufvarx, Laufvary) = False then
               Put ("   ");
            else
               Put (" * ");
            end if;
                        
         end loop;
         New_Line;
      end loop;
      
   end declarethis;

exception
   when Eingabeungueltig =>
      Put_Line ("Bitte keinen negativen Bereich angeben.");
      Put_Line ("Das Programm wird neu gestartet.");
      New_Line;
      Weihnachtsmann;
      
end Weihnachtsmann;
