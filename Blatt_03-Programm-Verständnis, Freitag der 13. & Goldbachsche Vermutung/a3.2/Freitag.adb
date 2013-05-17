--  FILE:    Freitag.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 3
--  VERSION: 1.0
--  DATE:    10.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 3.2: Freitag der 13.
--
--  Das Programm listet alle Freitage eines Jahres auf, die auf
--  den 13. eines Monats fallen.
--
-------------------------------------------------------------------
with Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Text_IO, Ada.Integer_Text_IO;
     
procedure Freitag is
   --  Den Monaten Januar bis Dezember werden die Anzahl der Tage zugeordnet.
   Monate : array (1 .. 12) of Integer := (31, 28, 31, 30, 31, 30, 31, 31, 30,
   31, 30, 31);
   Datum : Integer := 1;
   Tag : Integer := 0;
   Jahreszahl : Integer := 3000;
   Wochentag : String := "xx";
   Schaltjahreingabe : String := "n";
begin

   --  Der Benutzer gibt eine Jahreszahl zwischen der Einführung des
   --  gregorianischen Kalenders 1582 und dem Jahr 2999 ein.
   while Jahreszahl not in 1582 .. 2999 loop
      Ada.Text_IO.Put ("Jahreszahl: ");
      Get (Jahreszahl);
   end loop;

   while Tag not in 1 .. 7 loop
      if Wochentag = "Mo" or Wochentag = "mo" then
         Tag := 1;
      elsif Wochentag = "Di" or Wochentag = "di" then
         Tag := 2;
      elsif Wochentag = "Mi" or Wochentag = "mi" then
         Tag := 3;         
      elsif Wochentag = "Do" or Wochentag = "do" then
         Tag := 4;
      elsif Wochentag = "Fr" or Wochentag = "fr" then
         Tag := 5;
      elsif Wochentag = "Sa" or Wochentag = "sa" then
         Tag := 6;
      elsif Wochentag = "So" or Wochentag = "So" then
         Tag := 7;
      else
         Put ("Wochentag des 1.1. in diesem Jahr (Mo/Di/Mi/Do/Fr/Sa/So): ");
         Get (Wochentag);
      end if;
   end loop;

   Ada.Text_IO.Put ("Ist das Jahr ein Schaltjahr? (y/n): ");
   Get (Schaltjahreingabe);
   
   --  Falls das Jahr ein Schaltjahr ist hat der Februar 29 Tage.
   if Schaltjahreingabe = "y" then
      Monate (2) := 29;
   end if;
   
   --  Die Schleife läuft für jeden Monat des Jahres.
   for Laufvar in Monate'First .. Monate'Last loop
      Datum := 1;
      --  Solange das Datum nicht größer als die Anzahl der Tage diesen Monats
      --  ist wird die Schleife fortgesetzt.
      while Datum <= Monate (Laufvar) loop
         --  Wenn der Tag ein Freitag und das Datum der 13. ist
         --  wird das Datum ausgegeben.
         if Tag = 5 and Datum = 13 then
            Put (Datum'Img & '.' & Laufvar'Img & '.' & Jahreszahl'Img);
            New_Line;
         end if;
         if (Tag + 1) = 8 then
            Tag := 1;
         else
            Tag := Tag + 1;
         end if;
         Datum := Datum + 1;
      end loop;
   end loop;

end Freitag;