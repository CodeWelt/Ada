--  FILE:    Testen.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 6
--  VERSION: 1.0
--  DATE:    09.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 6.3: Mehrdimensionale Felder
--
--  Das Programm verwendet die Prozedur Knight_Distance im Package
--  Chess um die Felder eines Schachbretts zu berechnen.
--  Zum Schluss werden alle Felder formatiert ausgegeben.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Integer_Text_IO, Chess, Ada.Characters.Handling;
use  Ada.Text_IO, Ada.Integer_Text_IO, Chess, Ada.Characters.Handling;

procedure Testen
is
   procedure Fehler
      (Name : String)
   is
   begin
      Put ("Exception: " & Name);
      New_Line;
      Put ("Ungültige Eingabe. Das Programm wird neu gestartet.");
      New_Line;
      Put ("Bitte geben Sie Koordinaten im angegebenen Bereich ein.");
      New_Line;
      New_Line;
      Testen;
   end Fehler;

   Buchstaben : constant String := "ABCDEFGH";

   foo2 : Board_Position;
   foo : Chess_Board;
   
   Horizontal : Character := 'Z';
   Vertical : Integer := Integer'Last;   
begin
   
   --  Benutzer wird zur Eingabe eins Buchstabens und einer
   --  Zahl aufgefordert.
   Put ("Aufgabe 6.3: Mehrdimensionale Felder v1.0");
   New_Line;
   Put ("Bitte geben Sie die Koordinaten des Startfelds an:");
   New_Line;
   New_Line;   

   while Horizontal not in 'A' .. 'H' and Horizontal not in 'a' .. 'h' loop
      Put ("Horizontal ('A' .. 'H'): ");
      Get (Horizontal);
   end loop;

   while Vertical not in 1 .. 8 loop
      Put ("Vertical (1 .. 8): ");
      Get (Vertical);
   end loop;
   New_Line;
   
   foo2.H := To_Upper (Horizontal);
   foo2.V := Vertical;
   
   Knight_Distance (foo2, foo);

   --  Jedes Feld vom Chess_Board wird ausgegeben.
   for Laufvar in reverse 1 .. 8 loop
      Put (Laufvar'Img & " |");
      for LaufvarB in 1 .. 8 loop
         Put (foo (Buchstaben (LaufvarB), Laufvar), 2);
      end loop;
      New_Line;
   end loop;
   
   Put ("    ----------------");
   New_Line;
   Put ("     A B C D E F G H");

exception
   when Constraint_Error =>
      Fehler ("Constraint_Error");
   when Data_Error =>
      Fehler ("Data_Error");
end Testen;
