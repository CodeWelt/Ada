--  FILE:    Phonebook.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 6
--  VERSION: 1.0
--  DATE:    09.12.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 6.2: Telefonliste
--
--  Das Programm fragt den Benutzer nach einem Namensteil
--  (eine Zeichenkette) und sucht alle Personen aus der
--  Telefonliste, deren Name diese Zeichenkette enth�lt.
--  Das Programm gibt eine Liste der in Frage kommenden
--  Eintr�ge aus.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Strings.Unbounded, Ada.IO_Exceptions,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings;
use  Ada.Text_IO, Ada.Strings.Unbounded, Ada.IO_Exceptions,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings;

procedure Phonebook is

   --  TYPE Person
   --
   --  Jeder Telefonbucheintrag besteht aus dem Namen
   --  und Phonenumber der Person.
   type Person is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Phone : Ada.Strings.Unbounded.Unbounded_String;
      end record;
      
   Datei : File_Type;
   Telefonbuch : array (1 .. 100) of Person;
   Counter : Natural := 1;
   EintragCount : Natural := 0;
   Eingabe : Unbounded_String := Null_Unbounded_String;

   --  PROCEDURE Continue
   --  die Prozedur gibt alle Eintr�ge vom Telefonbuch
   --  die in Frage kommen aus.
   procedure Continue is
   begin
      EintragCount := 0;
      --  Die Schleife l�uft f�r jeden m�glichen Eintrag im Telefonbuch.
      for Laufvar in 1 .. 100 loop
         --  Wenn Ada.Strings.Unbounded.Count ein oder mehr vorkomnisse
         --  des Musters im Namen des aktuellen Eintrags gefunden hat,
         --  wird der Eintrag ausgegeben.
         if Ada.Strings.Unbounded.Count (Telefonbuch (Laufvar).Name,
         To_String (Eingabe)) > 0 then
            Put_Line (Telefonbuch (Laufvar).Name);
            Put_Line (Telefonbuch (Laufvar).Phone);
            EintragCount := EintragCount + 1;
         end if;
      
      end loop;
   
      if EintragCount = 0 then
         New_Line;
         Put_Line ("Der Namensteil " & Eingabe & " wurde nicht gefunden.");
      elsif EintragCount = 1 then
         New_Line;
         Put_Line ("Der Namensteil kam in einem Eintrag vor.");         
      else
         New_Line;
         Put_Line ("Der Namensteil kam in " & EintragCount'Img
          & " Eintr�gen vor.");
      end if;
   
   end Continue;
   

begin

   --  Die Datei wird mit dem File_Handle In_File zum lesen ge�ffnet.
   Open (Datei, In_File, "phonelist.txt");
   
   Put_Line ("Aufgabe 6.2: Telefonliste");
   Put ("Nach welchem Namensteil m�chten Sie suchen?: ");
   Get_Line (Eingabe);

   --  Die Schleife l�uft solange bis das Ende der ge�ffneten Datei
   --  erreicht wird. Wenn die Datei mehr als 100 Eintr�ge
   --  enth�lt, wird die Exception Constraint_Error raised.
   while not End_Of_File (Datei) loop
      Telefonbuch (Counter).Name := Get_Line (Datei);
      Telefonbuch (Counter).Phone := Get_Line (Datei);
      Counter := Counter + 1;
   end loop;
   
   Continue;

   exception
      --  Wenn beim �ffnen der Datei eine exception
      --  Ada.IO_Exceptions.Name_Error raised wurde,
      --  existiert die Datei nicht.
      when Ada.IO_Exceptions.Name_Error =>
         Put ("Die Datei phonelist.txt existiert nicht.");
         New_Line;
      --  Wenn beim Einlesen der Datei bzw beim speichern
      --  der eingelesenen Zeilen ein �berlauf
      --  Constraint_Error: ... index check failed
      --  auftritt, enth�lt die phonelist.txt mehr als 100 Eintr�ge.
      when Constraint_Error =>
         New_Line;
         Put ("Exception: Constraint_Error");
         New_Line;
         Put ("Die Datei phonelist.txt enth�lt mehr als 100 Eintr�ge.");
         New_Line;
         Put ("Es wird in den ersten 100 Eintr�gen gesucht.");
         New_Line;
         New_Line;
         Continue;

end Phonebook;
