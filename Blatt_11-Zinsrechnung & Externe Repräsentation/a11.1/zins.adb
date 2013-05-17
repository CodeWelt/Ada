--  FILE:    zins.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 11
--  VERSION: 1.0
--  DATE:    27.01.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
-- 
--  Aufgabe 11.1: Zinsrechnung
--
--  In dieser Aufgabe wird das momentane Guthaben auf einem
--  Sparkonto berechnet. Als Eingabe dienen zwei Textdateien,
--  einzahlung.txt und zinssatz.txt. In der Datei einzahlung.txt
--  werden Ein- und Auszahlungen auf das Konto mit jährlichen
--  Guthabenszinssatzes mit Datum der Änderung aufgelistet.
--  In der zinssatz.txt: Analog zu einzahlung.txt, jedoch wird
--  der Float-Wert als Zinssatz in Prozent pro Jahr interpretiert.
--  Das Package zins bietet Funktionalität rund um Zinsrechnung.
--
-------------------------------------------------------------------
with Ada.Float_Text_IO, Ada.Strings.Fixed,
     Ada.Strings.Unbounded.Text_IO, Ada.Strings.Unbounded, Dates,
     Ada.Text_IO, Ada.Unchecked_Deallocation;
use  Ada.Strings.Unbounded.Text_IO, Ada.Strings.Unbounded, Dates,
     Ada.Text_IO;

package body Zins is

   procedure Free is new Ada.Unchecked_Deallocation (Eintrag, Zeiger);

   --  Die globalen Zeiger auf die beiden Listen.
   ZahlungAnchor, ZinsAnchor : Zeiger := null;
   
   --  EXCEPTION Bad_Date
   --
   --  Wird immer dann erhoben wenn ein Datum (z.B.)
   --  nicht aus Punkten und numerischen Zeichen
   --  besteht.
   Bad_Date : exception;

   --  PROCEDURE Destroy
   --
   --  Es wird der Speicher beider Listen, ZahlungAnchor
   --  und ZinsAnchor, freigegeben und am ende die
   --  globalen Zeiger auf null gesetzt.
   procedure Destroy is
      ZahlungAnchorx, ZahlungAnchorxCopy : Zeiger := ZahlungAnchor;
      ZinsAnchorx, ZinsAnchorxCopy : Zeiger := ZinsAnchor;
   begin
      --  Die Liste ZahlungAnchor wird freigegeben.
      while ZahlungAnchorx /= null loop
         ZahlungAnchorxCopy := ZahlungAnchorx;
         ZahlungAnchorx := ZahlungAnchorx.Next;
         Free (ZahlungAnchorxCopy);
      end loop;
      ZahlungAnchor := ZahlungAnchorx;
      --  Die Liste ZinsAnchor wird freigegeben.
      while ZinsAnchorx /= null loop
         ZinsAnchorxCopy := ZinsAnchorx;
         ZinsAnchorx := ZinsAnchorx.Next;
         Free (ZinsAnchorxCopy);
      end loop;
      ZinsAnchor := ZinsAnchorx;
   end Destroy;
   
   --  FUNCTION Get_Date
   --
   --  Es wird das als Unbounded_String übergebene
   --  Datum zu einem typ Dates.Date aus dem Package
   --  Dates umgewandelt. Es wird die Exception
   --  Bad_Date erhoben wenn der String nicht
   --  konvertiert werden kann.
   --
   --  PARAMETERS:
   --  + Date_Text - Das Datum als Text welches
   --  zum echten Datum umgewandelt werden soll.
   --  RETURNS:
   --  Die Funktion liefert das übergebene Datum
   --  als typ Dates.Date zurück.
   function Get_Date
      (Date_Text : in Unbounded_String)
      return Dates.Date
   is
      Day_Dot, Month_Dot, Day, Month, Year : Integer := 0;
   begin
      --  Das Datum darf nicht weniger als 5 zeichen haben und
      --  nicht mehr als 10.
      if Length (Date_Text) < 5 or Length (Date_Text) > 10 then
         raise Bad_Date;
      end if;
      --  Es wird der erste Punkt von links gelesen gesucht und die Position
      --  in Day_Dot hinterlegt.
      Day_Dot := Ada.Strings.Fixed.Index (To_String (Date_Text), ".",
      Ada.Strings.Forward);
      --  Es wird der erste Punkt von rechts gelesen gesucht und die Position
      --  in Month_Dot hinterlegt.
      Month_Dot := Ada.Strings.Fixed.Index (To_String (Date_Text), ".",
      Ada.Strings.Backward);
      --  Es wird der Teil vom Datum als Text betrachtet der beim ersten
      --  Zeichen anfängt und bis zum Day_Dot geht. Danach wird dieser Teil
      --  zum Integer gewandelt und gespeichert. Wenn dabei ein
      --  Constraint_Error erhoben wird, ist ein nicht numerisches Zeichen
      --  dabei und das Datum somit nicht lesbar.
      Day := Integer'Value (Slice (Date_Text, 1, Day_Dot - 1));
      --  Ähnliche Vorgehensweise für Monat und Jahr.
      Month := Integer'Value (Slice (Date_Text, Day_Dot + 1, Month_Dot - 1));
      Year := Integer'Value (Slice (Date_Text, Month_Dot + 1,
      Length (Date_Text)));
      if Day > 31 or Day < 1 or Month > 12 or Month < 1 then
         raise Bad_Date;
      end if;
      --  Die gelesenen Integer werden mit hilfe der Create Funktion
      --  aus dem Package Dates zum typ Date gemacht und zurückgegeben.
      return Dates.Create (Day, Month, Year);
   exception
      when Constraint_Error =>
         raise Bad_Date;
   end Get_Date;

   --  FUNCTION Zerlege
   --
   --  Beim Einlesen der zwei Dateien wird
   --  jede Zeile in ihre Teile zerlegt.
   --  Um nacher mit den daraus gewonnenen Daten
   --  weiterarbeiten zu können.
   --
   --  PARAMETERS:
   --  + Line - Die aktuelle Zeile aus der Datei
   --  als Unbounded_String welche in ein Datum
   --  und einen Float-Wert zerlegt werden soll.
   --  RETURNS:
   --  Die Funktion liefert den in der zins.ads
   --  definierten Typ Date_Float zurück. Dieser
   --  Typ enthält die gewonnenen Daten.
   function Zerlege
      (Line : in Unbounded_String)
      return Date_Float
   is
      Zerlegt : Date_Float;
      Date_Text : Unbounded_String := Null_Unbounded_String;
      Space : Integer := 0;
   begin
      --  Es wird die Position der Leerzeile ermittelt die
      --  Datum und Float-Wert trennt.
      Space := Ada.Strings.Fixed.Index (To_String (Line), " ",
      Ada.Strings.Forward);
      Zerlegt.Zahl := Float'Value (Slice (Line, Space + 1, Length (Line)));
      Date_Text := To_Unbounded_String (Slice (Line, 1, Space - 1));
      --  Mit hilfe der Get_Date funktion die oben beschrieben ist
      --  wird ein typ Dates.Date gewonnen.      
      Zerlegt.Datum := Get_Date (Date_Text);
      --  Der nun mit Daten gefüllte Type Date_Float wird zurückgegeben.
      return Zerlegt;
   end Zerlege;

   
   --  PROCEDURE Run
   --
   --  Die Prozedur setzt das Startdatum auf den ersten
   --  Eintrag in der ZahlungAnchor Liste und führt solange
   --  Schritte aus bis das vom Benutzer eingegebene Datum
   --  erreicht ist.
   procedure Run is
      Eingabe : Unbounded_String := Null_Unbounded_String;
      EingabeDatum, DatumWalker : Dates.Date;
      ZahlungAnchorCopy : Zeiger := ZahlungAnchor;
      ZinsAnchorCopy : Zeiger := ZinsAnchor;
      Guthaben, Zinssatz, Zinsen : Float := 0.00;
      GibZinsen : Boolean := False;
   begin
      Put ("Berechnung bis (Datum): ");
      Get_Line (Eingabe);
      EingabeDatum := Get_Date (Eingabe);
      --  Das Startdatum wird auf das Datum des ersten
      --  Eintrags in der ZahlungAnchor Liste gesetzt.
      DatumWalker := ZahlungAnchorCopy.Datum;      
      while Dates."<" (DatumWalker, EingabeDatum) loop
         --  Zinserträge werden für den Zeitraum bis 30.12 jedes Jahres
         --  berechnet und am 01.01. des darauffolgenden Jahres wie eine
         --  gewöhnliche Einzahlung gutgeschrieben.
         if GibZinsen = True then
            Put ("Zinsgutschrift am " & Dates.Image (DatumWalker) & ": ");
            Ada.Float_Text_IO.Put (Zinsen, 0, 2, 0);
            New_Line;
            Guthaben := Guthaben + Zinsen;
            Zinsen := 0.00;
            GibZinsen := False;
         end if;
         --  Falls an einem Tag mehrere Kontobewegungen stattfinden
         --  wird hier solange ausgefüht bis alle am aktuellen Tag
         --  stattgefundenen Bewegungen ausgeführt sind.
         while ZahlungAnchorCopy.Datum = DatumWalker loop
            --  Das Konto kann nie negativ werden.
            if ZahlungAnchorCopy.Zahl < 0.00 and
            abs (ZahlungAnchorCopy.Zahl) > Guthaben then
               Put ("Fehlgeschlagene Buchung am " &
               Dates.Image (DatumWalker) & ": ");
               Ada.Float_Text_IO.Put (ZahlungAnchorCopy.Zahl, 0, 2, 0);
               New_Line;
            else
               Put ("Kontobewegung am " & Dates.Image (DatumWalker) & ": ");
               Ada.Float_Text_IO.Put (ZahlungAnchorCopy.Zahl, 0, 2, 0);
               New_Line;
               Guthaben := Guthaben + ZahlungAnchorCopy.Zahl;
            end if;
            if ZahlungAnchorCopy.Next = null then
               exit;
            else
               ZahlungAnchorCopy := ZahlungAnchorCopy.Next;
            end if;
         end loop;
         --  Es wird geprüft ob in der Liste ZinsAnchor für den aktuellen
         --  Tag eine Zinssatzänderung gemacht wird.
         if ZinsAnchorCopy.Datum = DatumWalker then
            Put ("Zinssatzänderung am " & Dates.Image (DatumWalker) & " auf ");
            Ada.Float_Text_IO.Put (ZinsAnchorCopy.Zahl, 0, 2, 0);
            Put (" % p.a.");
            New_Line;
            --  Wenn ja wird der neue Zinssatz ab sofort verwendet.
            Zinssatz := ZinsAnchorCopy.Zahl;
            if ZinsAnchorCopy.Next /= null then
               ZinsAnchorCopy := ZinsAnchorCopy.Next;
            end if;
         end if;
         --  Die Zinsen werden täglich ermittelt.
         Zinsen := Zinsen + (Guthaben * (Zinssatz / 36000.00));
         if Month (DatumWalker) = 12
         and DatumWalker = End_Of_Month (DatumWalker) then
            GibZinsen := True;
         end if;
         DatumWalker := Dates."+" (DatumWalker, 1);
      end loop;
      --  Wird das Sparkonto aufgelöst, so werden Zinsen bis zum Tag vor
      --  der Auflösung berechnet und mit ausgezahlt.
      Put ("Zinsgutschrift am " & Dates.Image (EingabeDatum) & ": ");
      Ada.Float_Text_IO.Put (Zinsen, 0, 2, 0);
      New_Line;
      Guthaben := Guthaben + Zinsen;
      Zinsen := 0.00;
      Put ("Guthaben bei Kontoauflösung am " & Dates.Image (EingabeDatum) &
      ": ");
      Ada.Float_Text_IO.Put (Guthaben, 0, 2, 0);
      New_Line;
   exception
      when Bad_Date =>
         Put_Line ("Bitte das Datum korrekt eingeben (e.g. 1.1.2007)");
         Run;
   end Run;


   --  PROCEDURE Load_einzahlung_txt
   --
   --  In der Prozedur Load_einzahlung_txt wird die Datei
   --  Zeile für Zeile eingelesen, zerlegt und die Bestandteile
   --  der Zeile in einen neuen Eintrag in der Liste ZahlungAnchor
   --  für die weitere Verwendung gespeichert.
   procedure Load_einzahlung_txt is
      File : Ada.Text_IO.File_Type;
      AktuelleZeile : Unbounded_String := Null_Unbounded_String;
      Zerlegt : Date_Float;
      Last : Zeiger := ZahlungAnchor;
   begin
      Put_Line ("Lesen von einzahlung.txt...");
      Open (File, in_file, "einzahlung.txt");
      while not End_Of_File (File) loop
         Get_Line (File, AktuelleZeile);
         Zerlegt := Zerlege (AktuelleZeile);
         if ZahlungAnchor = null then
            --  falls zuvor leere Liste, dann einzellige Liste erzeugen
            ZahlungAnchor := new Eintrag'(Datum => Zerlegt.Datum,
            Zahl => Zerlegt.Zahl, Next => null);
         else
            --  letzten Eintrag suchen
            Last := ZahlungAnchor;
            while Last.Next /= null loop
               Last := Last.Next;
            end loop;
            --  ... und Next des letzten Eintrags auf einen neuen Eintrag
            --  setzen
            Last.Next := new Eintrag'(Datum => Zerlegt.Datum,
            Zahl => Zerlegt.Zahl, Next => null);
         end if;
      end loop;
      Close (File);
   end Load_einzahlung_txt;


   --  PROCEDURE Load_zinssatz_txt
   --
   --  In der Prozedur Load_zinssatz_txt wird die Datei
   --  Zeile für Zeile eingelesen, zerlegt und die Bestandteile
   --  der Zeile in einen neuen Eintrag in der Liste ZinsAnchor
   --  für die weitere Verwendung gespeichert.
   procedure Load_zinssatz_txt is
      File : Ada.Text_IO.File_Type;
      AktuelleZeile : Unbounded_String := Null_Unbounded_String;
      Zerlegt : Date_Float;
      Last : Zeiger := ZinsAnchor;
   begin
      Put_Line ("Lesen von zinssatz.txt...");
      Open (File, in_file, "zinssatz.txt");

      while not End_Of_File (File) loop
         Get_Line (File, AktuelleZeile);
         Zerlegt := Zerlege (AktuelleZeile);

         if ZinsAnchor = null then
            --  falls zuvor leere Liste, dann einzellige Liste erzeugen
            ZinsAnchor := new Eintrag'(Datum => Zerlegt.Datum,
            Zahl => Zerlegt.Zahl, Next => null);
         else
            --  letzten Eintrag suchen
            Last := ZinsAnchor;
            while Last.Next /= null loop
               Last := Last.Next;
            end loop;
            --  ... und Next des letzten Eintrags auf einen neuen Eintrag
            --  setzen
            Last.Next := new Eintrag'(Datum => Zerlegt.Datum,
            Zahl => Zerlegt.Zahl, Next => null);
         end if;
      end loop;
      Close (File);
   end Load_zinssatz_txt;

end Zins;
