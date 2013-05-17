--  FILE:    mirror.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 3
--  VERSION: 1.0
--  DATE:    17.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 3.1: Programm-Verstaendnis
--
--  Das Programm berechnet die Spiegelworte eines gegebenen Textes.
--
-------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO; 

procedure Mirror
is
   Buffer : String (1 .. 1_001);
   Last : Natural; 

   --  FUNCTION Invert
   --  Die rekursive Funktion Invert berechnet die Spiegelworte
   --  des als String übergebenen Textes und gibt diese als
   --  String wieder zurück.
   --
   --  PARAMETERS:
   --  T ist der Text als String dessen Spiegelworte berechnet werden sollen.
   --
   --  RETURNS: Die Funktion gibt die berechneten Spiegelworte als
   --  String zurück.
   function Invert
      (T : in String)
      return String
   is
   begin
      --  Abbruchbedingung: Falls keine weiteren Zeichen mehr zu invertieren
      --  sind wird die rekursion beendet.
      if (T'Last - 1) /= 0 then
         --  Das letzte Zeichen wird durch Konkatenation an den Anfang vom
         --  noch zu invertierenden Teil gehängt. Dann wird mit den restlichen
         --  Zeichen rekursiv fortgefahren.
         return T (T'Last) & Invert (T (T'First .. T'Last - 1));
      else
         return T (T'Last) & ' ';
      end if;
   end Invert;

begin
   Put ("zu invertierender Text: ");
   Get_Line (Buffer, Last);
   --  Der rekursive Funktion Invert wird der vom Benutzer eingegebenen
   --  Text übergeben. Der zurückgegebene Text wird ausgegeben.
   Put_Line (Invert (Buffer (1 .. Last)));
end Mirror;
