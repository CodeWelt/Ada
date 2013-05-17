--  FILE:    File_Stats.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 4
--  VERSION: 1.0
--  DATE:    22.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 4.1: Datei-Statistiken
--
--  Das Programm analysiert Text-Dateien. Es wird die
--  durchschnittliche Zeilenlaenge, Kuerzeste Zeile,
--  Laengste Zeile, Anzahl der Zeilen und Anzahl der
--  Strichkommas ermittelt und ausgegeben.
--  Zur Eingabe für das Programm können auch ein oder mehrere
--  Kommandozeilen-Argumente übergeben werden.
--  Wird kein Kommandozeilen-Argument angegeben, so verlangt
--  das Programm vom Benutzer eine Eingabe.
--
-------------------------------------------------------------------

with Ada.Text_IO, Ada.Strings.Unbounded, Ada.IO_Exceptions,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings, Ada.Command_Line;
use  Ada.Text_IO, Ada.Strings.Unbounded, Ada.IO_Exceptions,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings, Ada.Command_Line;

procedure File_Stats is

   --  PROCEDURE Analyze
   --  Die Prozedur analysiert Text-Dateien. Es wird die
   --  durchschnittliche Zeilenlaenge, Kuerzeste Zeile,
   --  Laengste Zeile, Anzahl der Zeilen und Anzahl der
   --  Strichkommas ermittelt und ausgegeben.
   --
   --  PARAMETERS:
   --  Filename: Dies ist der Dateiname als String der
   --  Datei die Analysiert werden soll.
   procedure Analyze 
      (Filename : in String)
   is
      Datei : File_Type;
      ActiveLine : Unbounded_String := Null_Unbounded_String;
      KuerzesteZeile : Integer := Integer'Last;
      LaengsteZeile : Integer := 0;
      
      SKCount : Integer := 0;
      LineCount : Integer := 0;
      LaengeAllerZeilen : Integer := 0;
   begin
   
      New_Line;
      Put_Line ("Analysierte Datei: " & Filename);
      --  Die Datei wird mit dem File_Handle In_File zum lesen geöffnet.
      Open (Datei, In_File, Filename);
      
      --  Die Schleife läuft solange bis das Ende der geöffneten Datei
      --  erreicht wird.
      while not End_Of_File (Datei) loop
         --  Die aktuelle Zeile in der Datei wird eingelesen.
         ActiveLine := Get_Line (Datei);
         
         --  Die Kuerzeste Zeile der Datei wird ermittelt.
         if Length (ActiveLine) < KuerzesteZeile then
            KuerzesteZeile := Length (ActiveLine);
         end if;
         
         --  Die Laengste Zeile der Datei wird ermittelt.
         if Length (ActiveLine) > LaengsteZeile then
            LaengsteZeile := Length (ActiveLine);
         end if;
         
         --  Die Schleife läuft für jedes Zeichen der aktuellen Zeile.
         for Laufvar in 1 .. Length (ActiveLine) loop
            --  Wenn ein Strichkomma gefunden wurde, wird der Zähler um
            --  eins erhöht.
            if Element (ActiveLine, Laufvar) = ';' then
               SKCount := SKCount + 1;
            end if;
         end loop;
         
         --  Die Länge aller Zeilen und die Anzahl der Zeilen wird ermittelt
         --  um nach der Schleife die Durchschnittliche Zeilenlänge
         --  (LaengeAllerZeilen / LineCount) zu berechnen.
         LaengeAllerZeilen := LaengeAllerZeilen + Length (ActiveLine);
         LineCount := LineCount + 1;
      end loop;
      
      --  Wenn keine Zeilen eingelesen wurden ist die Datei leer.
      if LineCount = 0 then
         Put_Line ("Die Datei ist leer.");
      else
         --  Die zuvor ermittelten Daten Durchschnittliche Zeilenlänge,
         --  Kuerzeste Zeile, Laengste Zeile, Anzahl der Zeilen und
         --  Anzahl der Strichkomma werden ausgegeben.
         Put_Line ("Durchschnittliche Zeilenlaenge: " &
         Integer'Image (LaengeAllerZeilen / LineCount) & " Zeichen");
         Put_Line ("Kuerzeste Zeile: " & Integer'Image (KuerzesteZeile) &
         " Zeichen");
         Put_Line ("Laengste Zeile: " & Integer'Image (LaengsteZeile) &
         " Zeichen");
         Put_Line ("Anzahl Zeilen: " & Integer'Image (LineCount));
         Put_Line ("Anzahl ';': " & Integer'Image (SKCount));
      end if;
      
      Close (Datei);
      
      exception
         --  Wenn beim öffnen der Datei eine exception
         --  Ada.IO_Exceptions.Name_Error raised wurde,
         --  existiert die Datei nicht.
         when Ada.IO_Exceptions.Name_Error =>
            Put ("Die Datei " & Filename & " existiert nicht.");
            New_Line;
         when Ada.IO_Exceptions.Status_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;
         when Ada.IO_Exceptions.Mode_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;         
         when Ada.IO_Exceptions.Use_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;
         when Ada.IO_Exceptions.Device_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;
         when Ada.IO_Exceptions.End_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;
         when Ada.IO_Exceptions.Data_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;
         when Ada.IO_Exceptions.Layout_Error =>
            Put ("Kann nicht auf die Datei zugreifen.");
            New_Line;
         when others =>
            Put ("Fehler.");
            New_Line;
   end Analyze;
   
   Eingabe : Unbounded_String := Null_Unbounded_String;

begin

   --  Falls kein Kommandozeilen-Argument angegeben wurde,
   --  wird der Benutzer aufgefordert einen Dateinamen einzugeben.
   if Argument_Count = 0 then
      Put ("Bitte gib einen Dateinamen ein: ");
      Get_Line (Eingabe);
      Analyze (To_String (Eingabe));
   else
      --  Wenn ein oder mehr Kommandozeilen-Argumente angegeben wurden,
      --  läuft die Schleife für jeden angegebenen Dateinamen.
      for Laufvar in 1 .. Argument_Count loop
         Analyze (Argument (Laufvar));
      end loop;
   end if;

end File_Stats;
