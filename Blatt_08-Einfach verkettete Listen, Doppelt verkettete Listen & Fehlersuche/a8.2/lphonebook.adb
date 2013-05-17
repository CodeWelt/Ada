--  FILE:    lphonebook.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 8
--  VERSION: $Revision: 220 $
--  DATE:    $Date: 2006-12-15 11:50:39 +0100 (Fri, 15 Dec 2006) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------
--
--  Werkzeug, das Eintraege aus einer Telefonkartei liest und dem
--  Benutzer nach Eintraegen suchen laesst.
--


with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Person_Lists;

procedure LPhonebook
is

   --  EXCEPTION Read_Problem
   --
   --  Wird von Input_Phonebook propagiert, falls das Telefonbuch
   --  nicht gelesen werden konnte.
   Read_Problem : exception;


   --  PROCEDURE Input_Phonebook
   --
   --  Liest das Telefonbuch aus der Datei mit Namen 'File_Name' und
   --  fuegt die Eintraege in die Liste 'List' ein.  Gibt
   --  Fehlermeldungen aus, falls das Lesen der Datei nicht moeglich
   --  ist und propagiert Read_Error.
   --
   --  PARAMETERS:
   --  * List - Liste in die Eintraege eingefuegt werden sollen, muss
   --  Initialisiert sein.
   --  * File_Name - Name der Telefonbuch-Datei
   --
   --  RAISES:
   --  * Read_Problem - falls das Lesen des Telefonbuchs aus
   --  irgendeinem Grund nicht durchgefuehrt werden konnte
   procedure Input_Phonebook
     (List      : in out Person_Lists.Cell_Ref;
      File_Name : in     String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      File    : Ada.Text_IO.File_Type;
      Element : Person_Lists.Person;
      Last    : Natural;
      Buffer  : String (1 .. 128);
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

      --  Schleife liest pro Iteration einen Personen-Eintrag
      while not Ada.Text_IO.End_Of_File (File) loop
         --  Erste Zeile des Eintrags ist der Name
         Element.Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         loop
            Ada.Text_IO.Get_Line (File, Buffer, Last);
            Element.Name := Element.Name & Buffer (Buffer'First .. Last);
            exit when Last < Buffer'Last;
         end loop;

         --  Zweite Zeile des Eintrags ist die Telefonnummer. Falls
         --  bereits das Dateiende erreicht ist, wird die Exception
         --  End_Error propagiert
         Element.Phone := Ada.Strings.Unbounded.Null_Unbounded_String;
         loop
            Ada.Text_IO.Get_Line (File, Buffer, Last);
            --  Hier kann End_Error propagiert werden!
            Element.Phone := Element.Phone & Buffer (Buffer'First .. Last);
            exit when Last < Buffer'Last;
         end loop;

         Person_Lists.Insert (List, Element);
      end loop;

      Ada.Text_IO.Close (File);

   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            File_Name & " wurde nicht gefunden.");
         raise Read_Problem;

      when Ada.Text_IO.Use_Error | Ada.Text_IO.Device_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            File_Name & " konnte nicht gelesen werden.");
         raise Read_Problem;

      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            File_Name & " hat ungerade Zeilenanzahl.");
         --  File ist noch geoeffnet und muss geschlossen werden
         Ada.Text_IO.Close (File);
         raise Read_Problem;
   end Input_Phonebook;


   --  FUNCTION Get_Pattern
   --
   --  Fordert den Benutzer zur Eingabe eines Namensteils auf, nachdem
   --  gesucht werden soll.
   --
   --  RETURNS: Teilstring, der in den Namen im Telefonbuch gesucht
   --  werden soll.
   function Get_Pattern
     return String
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Buffer : String (1 .. 100);
      Last   : Natural;
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Text_IO.Put ("Namensteil: ");
      loop
         Ada.Text_IO.Get_Line (Buffer, Last);
         Result := Result & Buffer (Buffer'First .. Last);
         exit when Last < Buffer'Last;
      end loop;
      return Ada.Strings.Unbounded.To_String (Result);
   end Get_Pattern;


   Contents : Person_Lists.Cell_Ref;
begin
   --  Erzeugen einer leeren Liste
   Person_Lists.Create (Contents);
   --  Lesen der Telefonbucheintraege, kann Read_Problem propagieren
   Input_Phonebook (Contents, "phonelist.txt");

   declare
      Pattern : constant String := Get_Pattern;
      Current : Person_Lists.List_Cursor;
      Element : Person_Lists.Person;
   begin
      if Pattern'Length > 0 then
         --  Verwende einen Cursor um die Liste zu traversieren
         Current := Person_Lists.First (Contents);
         while Person_Lists.Is_Valid (Current) loop
            Element := Person_Lists.Get_Element (Current);
            if Ada.Strings.Unbounded.Count (Element.Name, Pattern) > 0 then
               Ada.Text_IO.Put_Line
                 (Ada.Strings.Unbounded.To_String (Element.Name) & ": " &
                  Ada.Strings.Unbounded.To_String (Element.Phone));
            end if;
            Person_Lists.Forward (Current);
         end loop;
      end if;
   end;

   --  Freigeben der Liste
   Person_Lists.Destroy (Contents);

exception
   when Read_Problem =>
      --  Freigeben der Liste
      Person_Lists.Destroy (Contents);
end LPhonebook;
