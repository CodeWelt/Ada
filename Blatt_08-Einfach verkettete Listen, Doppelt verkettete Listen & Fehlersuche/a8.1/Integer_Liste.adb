--  FILE:    Integer_Liste.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 8
--  VERSION: 1.0
--  DATE:    06.01.2007
--  AUTHOR: http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 8.1: Einfach verkettete Listen
--
--  Dieses Programm dient zur Demonstration des Packages Liste.adb
--  und Liste.ads.
--
-------------------------------------------------------------------

with Ada.Text_IO, ada.command_line, Liste;
use  Ada.Text_IO, ada.command_line, Liste;

procedure Integer_Liste is
   Z : Zeiger;
begin

   if Argument_Count = 0 then
      Put_Line ("Bitte geben sie eine Datei als Parameter an die Zahlen");
      Put_Line ("getrennt durch Leerzeichen enthält.");
   else
      Load (Argument (1), Z);
      Put_Line ("Original Liste:");
      Put(Z);
      
      New_Line;
      Z := umkehren (Z);
      Put_Line ("Umkehrung:");
      Put (Z);
      
      New_Line;
      Put_Line ("Sortierte Liste:");
      Selection_Sort (Z);
      Put (Z);
      
      Freigeben (Z);
   end if;
          
end Integer_Liste;
