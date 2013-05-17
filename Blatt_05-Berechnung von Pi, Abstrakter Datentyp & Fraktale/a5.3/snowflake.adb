--  FILE:    Snowflake.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 5
--  VERSION: 1.0
--  DATE:    02.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 5.3: Fraktale
--
--  Als Kochsche Kurve bezeichnet man eine spezialle Kurve,
--  deren Funktion überall stetig aber nirgends
--  differenzierbar ist.
--  Das Programm berechnet eine Näherung an diese Kurve mit einem
--  rekursiven Algorithmus. Durch die Wahl einer bestimmten
--  Rekursionstiefe wird die Genauigkeit der Näherung bestimmt.
--
-------------------------------------------------------------------

with Ada.Command_Line, Adalogo;
use  Ada.Command_Line, Adalogo;

procedure Snowflake is

   --  PROCEDURE Fraktal
   --  Die rekursive Prozedur Fraktal berechnet und
   --  zeichnet die Näherung an die Kochsche Kurve.
   --
   --  PARAMETERS:
   --  Seitenlaenge: Dies ist die Seitenlaenge welche
   --  nach jeder Drehung von der Turtle gezeichnet wird.
   --  Rekursionstiefe: Dieser Parameter bestimmt
   --  die Abbruchbedingung, wann das Ende der rekursion
   --  festgelegt werden soll. Zum testen bitte eine
   --  kleine Rekursionstiefe wählen, da das Programm
   --  für größere Tiefen langsam wird.
   procedure Fraktal
      (Seitenlaenge : in Float;
       Rekursionstiefe : in Natural)
   is
   begin

      --  Abbruchbedingung
      if Rekursionstiefe = 0 then
         null;
      else
         Fraktal (Seitenlaenge, Rekursionstiefe - 1);
         Turn (60);  
         Forward (Seitenlaenge); 

         Fraktal (Seitenlaenge, Rekursionstiefe - 1);
         Turn (-120);
         Forward (Seitenlaenge);

         Fraktal (Seitenlaenge, Rekursionstiefe - 1);
         Turn (60);  
         Forward (Seitenlaenge);         

         Fraktal (Seitenlaenge, Rekursionstiefe - 1);
      end if;
      
   end Fraktal;

   Seitenlaenge : Float := 200.0;
   Rekursionstiefe : Natural := 3;
begin
   Turtle_Reset;

   --  Für den Fall das ein oder kein Kommandozeilen-Argument
   --  übergeben wurde, werden die Standartwerte verwendet.
   --  Werden mehr Kommandozeilen-Argumente übergeben, so werden
   --  die angegebenen Werte verwendet.
   if Argument_Count = 0 then
      null;
   else
      if Argument_Count = 1 then
         null;
      else
         --  Die Seitenlänge ist das erste Kommandozeilen-Argument.
         Seitenlaenge := Float'Value (Argument (1));
         --  Die Rekursionstiefe ist das zweite Kommandozeilen-Argument.
         Rekursionstiefe := Integer'Value (Argument (2));
      end if;   
   end if;

   --  Die Prozedur Fraktal wird drei Mal aufgerufen um für jede
   --  Seite des gleichseitigen Dreiecks eine Kochsche Kurve zu zeichnen.
   Forward (Seitenlaenge); 
   Fraktal (Seitenlaenge, Rekursionstiefe);

   Turn (-120);
   Forward (Seitenlaenge); 
   Fraktal (Seitenlaenge, Rekursionstiefe);   

   Turn (-120);
   Forward (Seitenlaenge); 
   Fraktal (Seitenlaenge, Rekursionstiefe);

   Draw;
      
end Snowflake;
