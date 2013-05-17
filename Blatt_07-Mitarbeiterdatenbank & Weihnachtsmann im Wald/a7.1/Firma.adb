--  FILE:    Firma.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 7
--  VERSION: 1.0
--  DATE:    17.12.2006
--  AUTHOR: http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 7.1: Mitarbeiterdatenbank
--
--  Das Package bietet Funktionalität für eine
--  Mitarbeiter-Datenbank. Bitte entnehmen Sie weitere
--  Informationen aus den Kommentaren zu jeder Prozedur oder
--  Funktion.
--
-------------------------------------------------------------------
with Ada.Text_IO, Ada.Numerics, Ada.Numerics.Discrete_Random,
     Ada.Strings.Unbounded.Text_IO, Ada.IO_Exceptions;
use  Ada.Text_IO, Ada.Strings.Unbounded.Text_IO, Ada.IO_Exceptions;

package body Firma is

   File : File_Type;
   Personaldaten : array (1 .. 100) of Person;

   --  PROCEDURE Zerlege
   --  Die Prozedur Zerlege wird von der Prozedur Loadpersonnel
   --  gebraucht um die gerade aus der Datei gelesene Zeile
   --  in ihre 3 Einzelteile (<key>#<name>#<boss>) zu zerlegen und
   --  in der Mitarbeiter-Datenbank abzulegen.
   --  PARAMETERS:
   --  Personalnummer, Name, Vorgesetzter sind die Werte der
   --  Person die belegt werden sollen.
   --  Source ist die gerade aus der Datei gelesene Zeile
   --  (<key>#<name>#<boss>) die zerlegt und abgelegt werden sollen.
   procedure Zerlege
      (Personalnummer : in out Integer;
       Name : in out Unbounded_String;
       Vorgesetzter : in out Integer;
       Source : in Unbounded_String)
   is
      Personalnummer_Unbounded_String : Ada.Strings.Unbounded.Unbounded_String
      := Null_Unbounded_String;
      Vorgesetzter_Unbounded_String : Ada.Strings.Unbounded.Unbounded_String
      := Null_Unbounded_String;
      Zerlegen : Integer range 1 .. 3 := 1;
   begin
      --  Die Schleife läuft für jeden Buchstaben im Source.
      for Laufvar in 1 .. Length (Source) loop
         --  Wenn das aktuelle Element ein Trennzeichen (hier die Raute) ist,
         --  wird der nächste Wert betrachtet.
         if Element (Source, Laufvar) = '#' then
            Zerlegen := Zerlegen + 1;
         else
            --  Jenachdem zwischen welchem der 2 Trennzeichen und dem Ende
            --  man sich befindet wird das aktuelle Zeichen an
            --  die bisher eingelesenen Zeichen dem entsprechenden
            --  Wert angehängt.
            case Zerlegen is
               when 1 => Personalnummer_Unbounded_String
               := Personalnummer_Unbounded_String & Element (Source, Laufvar);
               when 2 => Name := Name & Element (Source, Laufvar);
               when others => Vorgesetzter_Unbounded_String
               := Vorgesetzter_Unbounded_String & Element (Source, Laufvar);
            end case;
         end if;
      end loop;
      
      Personalnummer := Integer'Value (To_String
       (Personalnummer_Unbounded_String));
      Vorgesetzter := Integer'Value (To_String
       (Vorgesetzter_Unbounded_String));
   exception
      when Constraint_Error =>
         Put_Line ("Die Datenbank ist nicht korrekt formatiert.");
         Put_Line ("Die Zeile " & Source & " wurde übergangen.");
         New_Line;
   end Zerlege;

   --  PROCEDURE Loadpersonnel
   --  Aus der Datei personnel.txt wird gelesen, jede Zeile
   --  durch die Prozedur Zerlege in ihre 3 Einzelteile zerlegt
   --  und in der Mitarbeiter-Datenbank gespeichert.
   procedure Loadpersonnel is
      Counter : Natural := 0;
   begin
      Open (File, In_File, "personnel.txt");
      while not End_Of_File (File) loop
         Counter := Counter + 1;
         Zerlege (Personaldaten (Counter).Personalnummer,
          Personaldaten (Counter).Name,
         Personaldaten (Counter).Vorgesetzter, Get_Line (File));
      end loop;
      Close (File);
   exception
      --  Wenn beim Öffnen der Datei eine exception
      --  Ada.IO_Exceptions.Name_Error raised wurde,
      --  existiert die Datei nicht.
      when Ada.IO_Exceptions.Name_Error =>
         Put ("Die Datei personnel.txt existiert nicht.");
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
         Put ("Fehler in procedure Loadpersonnel.");
         New_Line;
   end Loadpersonnel;

   --  PROCEDURE SavepersonnelNeu
   --  Nachdem mit der Hire prozedur die eingelesene Mitarbeiter-Datenbank
   --  verändert wurde, wird hier eine Kopie erstellt und gespeichert.
   procedure SavepersonnelNeu is
      NoSpacesPersonalnummer : Ada.Strings.Unbounded.Unbounded_String
       := Null_Unbounded_String;
      NoSpacesVorgesetzter : Ada.Strings.Unbounded.Unbounded_String
       := Null_Unbounded_String;
      TempPersonalnummer : Ada.Strings.Unbounded.Unbounded_String
       := Null_Unbounded_String;
      TempVorgesetzter : Ada.Strings.Unbounded.Unbounded_String
       := Null_Unbounded_String;
   begin
      --  Die Datei wird zum Schreiben geöffnet.
      Create (File, Out_File, "personnel_neu.txt");
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Laufvar in Personaldaten'Range loop
         --  Wenn die aktuelle Person ein gesetzter Eintrag ist, wird
         --  fortgefahren.
         if Personaldaten (Laufvar).Personalnummer /= Integer'Last then
            --  Es folgen zwei ähnliche Schleifen die die Ausgaben der Integer
            --  Image Funktionen für die Personalnummer und den Vorgesetzten
            --  bearbeiten um danach in die Datenbank-Kopie keine 
            --  Leerzeichen zu schreiben.
            NoSpacesPersonalnummer := Null_Unbounded_String;
            TempPersonalnummer := To_Unbounded_String
             (Personaldaten (Laufvar).Personalnummer'Img);
            for Laufvar2 in 1 .. Length (TempPersonalnummer) loop
               if Element (TempPersonalnummer, Laufvar2) /= ' ' then
                  NoSpacesPersonalnummer := NoSpacesPersonalnummer &
                   Element (TempPersonalnummer, Laufvar2);
               end if;
            end loop;

            NoSpacesVorgesetzter := Null_Unbounded_String;
            TempVorgesetzter := To_Unbounded_String
            (Personaldaten (Laufvar).Vorgesetzter'Img);
            for Laufvar2 in 1 .. Length (TempVorgesetzter) loop
               if Element (TempVorgesetzter, Laufvar2) /= ' ' then
                  NoSpacesVorgesetzter := NoSpacesVorgesetzter &
                   Element (TempVorgesetzter, Laufvar2);
               end if;
            end loop;

            --  Es wird in die Datenbank-Kopie nach dem vorgegebenen
            --  Format <key>#<name>#<boss> geschrieben.
            Put_Line (File, NoSpacesPersonalnummer & "#" & To_String
             (Personaldaten (Laufvar).Name) & "#" & NoSpacesVorgesetzter);
         end if;
      end loop;
      Close (File);
      
   exception
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
         Put ("Fehler in procedure SavepersonnelNeu.");
         New_Line;

   end SavepersonnelNeu;
   
   --  FUNCTION Find
   --  Die Prozedur gibt Name und Personalnummer des direkten
   --  Vorgesetzten zu einem bestimmten Mitarbeiter aus.
   --  
   --  PARAMETERS:
   --  FindPersonalnummer: FindPersonalnummer ist die
   --  Personalnummer als Integer Wert der gesucht wird.   
   --  RETURNS:
   --  Es wird der Eintrag aus der Mitarbeiter-Datenbank
   --  zurückgegeben bei der die Perstonalnummer
   --  übereinstimmt.
   function Find (FindPersonalnummer : in Integer) return Person is
      NotFound : Person;
   begin
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Lauf in Personaldaten'Range loop
         --  Wenn die gesuchte Personalnummer mit der Perstonalnummer
         --  der aktuellen Person übereinstimmt, wird dieser Eintrag
         --  zurückgegeben.
         if Personaldaten (Lauf).Personalnummer = FindPersonalnummer then
            return Personaldaten (Lauf);
         end if;
      end loop;
      return NotFound;
   end Find;

   --  FUNCTION Find_String
   --  Die Prozedur gibt Name und Personalnummer des direkten
   --  Vorgesetzten zu einem bestimmten Mitarbeiter aus.
   --  
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  direkter Vorgesetzter gesucht wird.   
   --  RETURNS:
   --  Es wird der Eintrag aus der Mitarbeiter-Datenbank
   --  zurückgegeben bei der entweder Name oder Personalnummer
   --  übereinstimmt.
   function Find_String (FindThis : in String) return Person
   is
      NoSpaces : Ada.Strings.Unbounded.Unbounded_String
       := Null_Unbounded_String;
      Temp : Ada.Strings.Unbounded.Unbounded_String := Null_Unbounded_String;
      NotFound : Person;
   begin
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Lauf in Personaldaten'Range loop
         --  Nur wenn die aktuelle Person in der Mitarbeiter-Datenbank ein
         --  belegter Eintrag ist, wird fortgefahren.
         if Personaldaten (Lauf).Personalnummer /= Integer'Last then
            Temp := To_Unbounded_String
             (Personaldaten (Lauf).Personalnummer'Img);
            NoSpaces := Null_Unbounded_String;
            --  Es werden nur die Zeichen aus der Image Funktion
            --  weiterverwendet die keine Leerzeichen sind.
            for Laufvar in 1 .. Length (Temp) loop
               if Element (Temp, Laufvar) /= ' ' then
                  NoSpaces := NoSpaces & Element (Temp, Laufvar);
               end if;
            end loop;
            --  Wenn entweder der Name oder die Personalnummer der aktuellen
            --  Person übereinstimmt wird diese Person zurückgegeben.
            if NoSpaces = FindThis or Personaldaten (Lauf).Name = FindThis then
               return Personaldaten (Lauf);
            end if;
         end if;
      end loop;
      --  Wenn keine übereinstimmungen gefunden wurden, wird
      --  eine Leere Person, ohne gesetzten Werten, zurückgegeben.
      return NotFound;
   end Find_String;
   
   --  PROCEDURE List
   --  Die Prozedur gibt Name und Personalnummer aller Mitarbeiter
   --  in einer Liste aus.
   procedure List is
   begin
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Laufvar in Personaldaten'Range loop
         --  Nur wenn die aktuelle Person in der Mitarbeiter-Datenbank ein
         --  belegter Eintrag ist, wird ausgegeben.
         if Personaldaten (Laufvar).Personalnummer /= Integer'Last then
            Put_Line (Personaldaten (Laufvar).Personalnummer'Img &
            ": " & Personaldaten (Laufvar).Name);
         end if;
      end loop;
   end List;
   
   --  PROCEDURE Supervisor
   --  Die Prozedur gibt Name und Personalnummer des direkten
   --  Vorgesetzten zu einem bestimmten Mitarbeiter aus.
   --  
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  direkter Vorgesetzter gesucht wird.
   procedure Supervisor (Name : in String) is
      Found : Person;
      FoundVorgesetzter : Person;
   begin
      Found := Find_String (Name);   
      --  Wenn der Name nicht in der Mitarbeiter-Datenbank gefunden wurde,
      --  das bedeutet von der Suchfunktion Find_String wird eine Leerer
      --  (nicht gesetzter) Eintrag zurückgegeben, wird eine entsprechende
      --  Fehlermeldung ausgegeben.
      if Found.Personalnummer = Integer'Last then
         Put_Line ("Die Person " & Name &
          " wurde nicht in der Datenbank gefunden.");
      elsif Found.Vorgesetzter = 0 then
         Put_Line ("Die Person " & Found.Name &
          " gehört der Geschäftsführung an.");
         Put_Line ("Der Mitarbeiter hat keinen direkten Vorgesetzten.");
      else
         --  Es wird der direkte Vorgesetzte zur gerade gefundenen Person
         --  ermittelt und die Ausgabe wird durchgeführt.
         FoundVorgesetzter := Find (Found.Vorgesetzter);
         if FoundVorgesetzter.Personalnummer = Integer'Last then
            Put_Line ("Zur Person " & Name & " wurde der direkte Vorgesetzte");
            Put_Line ("mit der Personalnummer " & Found.Vorgesetzter'Img &
             " nicht gefunden.");
         else
            Put_Line ("Der direkte Vorgesetzte von " & Name & " ist:");
            Put_Line (FoundVorgesetzter.Personalnummer'Img & ": " &
             FoundVorgesetzter.Name);
         end if;
      end if;
      null;
   end Supervisor;
   
   --  PROCEDURE Peers
   --  Die Prozedur gibt eine Liste der Namen und Personalnummern
   --  der Kollegen eines als String übergebenen Mitarbeiters aus.
   --  Mit Kollegen sind hier nur diejenigen Mitarbeiter gemeint,
   --  die den selben direkten Vorgesetzten haben.
   --
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  Kollegen gelistet werden sollen.
   procedure Peers (Name : in String) is
      Found : Person;
   begin
      Found := Find_String (Name);      
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Laufvar in Personaldaten'Range loop
         --  Wenn die zuvor in der Mitarbeiter-Datenbank gefundene Person
         --  den selben Vorgesetzten hat wie die aktuelle Person, wird
         --  ausgegeben.
         if Personaldaten (Laufvar).Vorgesetzter = Found.Vorgesetzter and
          Personaldaten (Laufvar).Personalnummer /= Integer'Last then
            Put_Line (Personaldaten (Laufvar).Personalnummer'Img & ": " &
            Personaldaten (Laufvar).Name);
         end if;
      end loop;
   end Peers;
   
   --  PROCEDURE Hire
   --  Berechnet für jedes Feld auf dem Schachbrett, wieviele Zuege
   --  das Pferdchen mindestens benoetigt, um von dem Feld 'Start' aus
   --  auf dieses Feld zu ziehen. Speichert diese Information in dem
   --  out-Parameter 'Board'.
   --
   --  PARAMETERS:
   --  + Start - Startfeld des Springers
   --  + Board - Spielfeld, Abbildung von Feld zu Anzahl Zuege   
   procedure Hire (P1 : in String; P2 : in String) is
      Found : Person;
      FoundforRand : Person;
      HatWas : Boolean := False;
      subtype Rand_Range is Integer range 1 .. 100;
      package Rand is new Ada.Numerics.Discrete_Random (Rand_Range);
      use Rand;
      Gen : Generator;
      Random100 : Rand_Range := 1;
   begin
      Reset (Gen);
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Laufvar in Personaldaten'Range loop
         --  Die if Anweisung wird nur dann ausgeführt wenn der erste
         --  nicht gesetzte Eintrag erreicht wurde.
         if Personaldaten (Laufvar).Personalnummer = Integer'Last then
            --  Die Schleife läuft so lange bis die Personalnummer noch nicht
            --  gesetzt wurde. Das bedeutet es wird so lange gesucht bis eine
            --  Personalnummer gefunden wird die noch nicht verwendet wurde.
            while Personaldaten (Laufvar).Personalnummer = Integer'Last loop
               Random100 := Random (Gen);
               --  Es wird geschaut ob der eben generierte Wert zwischen
               --  1 und 100 bereits als Personalnummer in der Datenbank
               --  vorhanden ist. Wenn das nicht der Fall ist, also die
               --  von Procedure Find zurückgegebene Person hat
               --  keine gesetzte Personalnummer, wurde die generierte
               --  Personalnummer noch nicht verwendet und wird dem
               --  neuen Eintrag in der veränderten Datenbank zugewiesen und
               --  die while Schleife wird beendet.
               FoundforRand := Find (Random100);
               if FoundforRand.Personalnummer = Integer'Last then
                  Personaldaten (Laufvar).Personalnummer := Random100;
               end if;
            end loop;

            Personaldaten (Laufvar).Name := To_Unbounded_String (P2);
            --  Es wird der Eintrag des direkten Vorgesetzten gesucht
            --  und dessen Personalnummer als Vorgesetzter des neuen
            --  Mitarbeiters gesetzt.
            Found := Find_String (P1);
            if Found.Personalnummer = Integer'Last then
               Put_Line ("Die Person " & P1 &
                " wurde in der Mitarbeiter-Datenbank");
               Put_Line ("nicht gefunden. " & P2 &
                " wird als Geschäftsführer hinzugefügt.");
               Personaldaten (Laufvar).Vorgesetzter := 0;
            else
               Personaldaten (Laufvar).Vorgesetzter := Found.Personalnummer;
            end if;
            HatWas := True;
            exit;
         end if;
      end loop;

      if HatWas = False then
         Put_Line ("Die Datenbank enthält bereits 100 Einträge.");
         Put_Line (P2 & " konnte nicht hinzugefügt werden.");
      else
         Put_Line ("Hired " & P2 & ", reporting to " & P1 & ".");
         SavepersonnelNeu;
         Put_Line
          ("Die neue Datenbank wurde in personnel_neu.txt gespeichert.");
      end if;
   end Hire;

   --  PROCEDURE Team
   --  Die rekursive Prozedur gibt eine Liste der Namen und
   --  Personalnummern der Mitarbeiter mit (möglicherweise
   --  indirektem) Vorgesetztem aus.
   --
   --  PARAMETERS:
   --  Name: Name identifiziert die Person als String dessen
   --  Vorgesetzter gefunden werden soll.
   procedure Team (Name : in String) is
      Found : Person;
   begin
      Found := Find_String (Name);
      --  Die Schleife läuft für alle Personen in der Mitarbeiter-Datenbank.
      for Laufvar in Personaldaten'Range loop
         --  Wenn der Vorgesetzter der aktuellen Person die Personalnummer
         --  der zuvor gesuchten Person ist, wird fortgefahren.
         if Personaldaten (Laufvar).Vorgesetzter = Found.Personalnummer and
         Personaldaten (Laufvar).Vorgesetzter /= Integer'Last then
            Put_Line (Personaldaten (Laufvar).Personalnummer'Img & ": " &
            Personaldaten (Laufvar).Name);
            --  Solange noch gesetzte Einträge in der Mitarbeiter-Datenbank
            --  sind (Abbruchbedingung), wird die Prozedur Team rekursiv
            --  für die aktuelle Person aufgerufen.
            if Personaldaten (Laufvar).Personalnummer /= Integer'Last then
               Team (To_String (Personaldaten (Laufvar).Name));
            end if;
         end if;
      end loop;
   end Team;
   
end Firma;
