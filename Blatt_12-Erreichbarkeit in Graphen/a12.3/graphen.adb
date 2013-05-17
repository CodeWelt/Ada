--  FILE:    graphen.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 12
--  VERSION: 1.0
--  DATE:    04.02.2007
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
-- 
--  Aufgabe 12.3: Erreichbarkeit in Graphen
--
--  In dieser Aufgabe wird ein gerichteter Graph aus der Datei
--  graph.txt geladen und mit Hilfe von Adjazenzlisten
--  repräsentiert. Aus dieser Darstellung wird eine
--  Adjazenzmatrix des Graphen erstellt.
--  Die graph.txt ist wie folgt aufgebaut: In jeder Zeile
--  der Datei ist eine Kante spezifiziert mit Startknoten-Id,
--  Kantengewicht und Zielknoten-Id.
--  Eine Zeile hat folgendes Format, wobei Source für die
--  Knoten-Id des Quellknotens steht, Target für die Knoten-Id
--  des Zielknotens und W für das Gewicht der Kante:
--  Source (W) Target
--
-------------------------------------------------------------------
with Ada.Text_IO, Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO,
     Ada.Strings.Fixed, Ada.Unchecked_Deallocation;
use  Ada.Text_IO, Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO;

package body graphen is

   --  Zwei Instanzen von Ada.Unchecked_Deallocation für die zwei
   --  unterschiedlichen Listen Knotenliste und Kantenliste.
   procedure FreeKnoten is new Ada.Unchecked_Deallocation
   (Knoten, Knotenliste);
   procedure FreeKante is new Ada.Unchecked_Deallocation (Kante, Kantenliste);

   --  Der Anchor zur Knotenliste als globale Variable.
   KnotenlisteAnchor : Knotenliste := null;
   --  Es wird beim Hinzufügen von Elementen in die Knotenliste
   --  die Anzahl der Elemente in der Knotenliste gezählt.
   KnotenlisteCounter : Integer := 0;
   --  Unter allen hinzugefügten Knoten wird das Element gesucht
   --  mit der Längsten Id um später die Matrix formatiert
   --  auszugeben.
   LaengsteId : Integer := 0;


   --  FUNCTION Is_Empty
   --
   --  Es wird ermittelt ob der übergebene Zeiger
   --  auf eine Knotenliste leer ist oder nicht.
   --
   --  PARAMETERS:
   --  + Anchor - Zeiger auf eine Knotenliste.
   --  RETURNS:
   --  Die Funktion liefert True zurück wenn der
   --  übergebene Zeiger null ist, andernfalls
   --  False.
   function Is_Empty
     (Anchor : in Knotenliste)
     return Boolean
   is
   begin
      return Anchor = null;
   end Is_Empty;


   --  FUNCTION Zerlege
   --
   --  Beim Einlesen der Datei wird
   --  jede Zeile in ihre Teile zerlegt.
   --  Um nacher mit den daraus gewonnenen Daten
   --  weiterarbeiten zu können.
   --
   --  PARAMETERS:
   --  + Line - Die aktuelle Zeile aus der Datei
   --  als Unbounded_String welche in Knoten-Id des
   --  Quellknotens, Gewicht der Kante und Knoten-Id
   --  des Zielknotens zerlegt werden soll.
   --  RETURNS:
   --  Die Funktion liefert den in der graphen.ads
   --  definierten Typ SourceWTarget zurück. Dieser
   --  Typ enthält die gewonnenen Daten.
   function Zerlege
      (Line : in Unbounded_String)
      return SourceWTarget
   is
      KlammerAufPos, SpacePos, KlammerZuPos : Integer := 0;
      Zerlegt : SourceWTarget;
   begin
      --  Die Positionen der Klammeren und der Leerzeile werden
      --  bestimmt.
      KlammerAufPos := Ada.Strings.Fixed.Index (To_String (Line), "(",
      Ada.Strings.Forward);
      KlammerZuPos := Ada.Strings.Fixed.Index (To_String (Line), ")",
      Ada.Strings.Forward);
      SpacePos := Ada.Strings.Fixed.Index (To_String (Line), " ",
      Ada.Strings.Forward);
      --  Die aus der übergebenen Zeile ermittelten Daten werden
      --  in den Typ SourceWTarget hinterlegt und zurückgegeben.
      Zerlegt.W := Natural'Value (Slice (Line, KlammerAufPos + 1,
      KlammerZuPos - 1));
      Zerlegt.Target := To_Unbounded_String (Slice (Line, KlammerZuPos + 2,
      Length (Line)));
      Zerlegt.Source := To_Unbounded_String (Slice (Line, 1, SpacePos - 1));
      return Zerlegt;
   end Zerlege;


   --  FUNCTION In_Knotenliste
   --
   --  Beim Einlesen der Datei wird
   --  gefragt ob der aktuell betrachtete Knoten
   --  bereits in der Liste der Knoten vorhanden
   --  ist oder nicht.
   --
   --  PARAMETERS:
   --  + Idx - Die Id des aktuell betrachteten
   --  Knotens als Unbounded_String zum
   --  Identifizieren und Vergleichen.
   --  RETURNS:
   --  Die Funktion liefert True zurück wenn
   --  die übergebene Knoten-Id bereits in der
   --  Liste vorhanden ist, andernfalls False.
   function In_Knotenliste
      (Idx : in Unbounded_String)
      return Boolean
   is
      Check : Knotenliste := KnotenlisteAnchor;
   begin
      if Is_Empty (KnotenlisteAnchor) = True then
         return False;
      end if;
      Check := KnotenlisteAnchor;
      --  Die Schleife läuft durch alle Elemente der
      --  Knotenliste.
      while Check /= null loop
         --  Wenn die Id des Elements gleich ist wie
         --  der übergebene Unbounded_String, welcher
         --  gesucht wird, wird True zurückgegeben.
         if Check.Id = Idx then
            return True;
         end if;
         Check := Check.Next;
      end loop;
      --  Wenn keine übereinstimmung gefunden wurde,
      --  ist die gesuchte Id nicht in der Liste
      --  vorhanden und es wird False zurückgegeben.
      return False;
   end In_Knotenliste;
   

   --  PROCEDURE AddToKnotenliste
   --
   --  Es wird ein neues Element mit der
   --  als Unbounded_String übergebenen Id 
   --  an die Knotenliste angefügt.
   --
   --  PARAMETERS:
   --  + New_Id - Die Id des neuen Elements.
   procedure AddToKnotenliste
     (New_Id : in Unbounded_String)
   is
      Last : Knotenliste := KnotenlisteAnchor;
   begin
      --  Unter allen hinzugefügten Knoten wird das Element gesucht
      --  mit der Längsten Id um später die Matrix formatiert
      --  auszugeben.
      if Length (New_Id) > LaengsteId then
         LaengsteId := Length (New_Id);
      end if;
      --  Es wird beim Hinzufügen von Elementen in die Knotenliste
      --  die Anzahl der Elemente in der Knotenliste gezählt.
      KnotenlisteCounter := KnotenlisteCounter + 1;
      if Is_Empty (KnotenlisteAnchor) then
         --  falls zuvor leere Liste, dann einzellige Liste erzeugen
         KnotenlisteAnchor := new Knoten'(Next => null, Kanten => null,
         Id => New_Id);
      else
         --  letzte Zelle suchen
         Last := KnotenlisteAnchor;
         while Last.Next /= null loop
            Last := Last.Next;
         end loop;
         --  ... und Next der letzten Zelle auf eine neue Zelle setzen
         Last.Next := new Knoten'(Next => null, Kanten => null, Id => New_Id);
      end if;
   end AddToKnotenliste;


   --  PROCEDURE Load
   --
   --  In dieser Prozedur wird die Datei graph.txt gelesen
   --  und die daraus resultierenden Listen ermittelt.
   procedure Load is
      File : Ada.Text_IO.File_Type;
      AktuelleZeile : Unbounded_String := Null_Unbounded_String;
      Zerlegt : SourceWTarget;
      Abbruch : Boolean := False;
      KnotenlisteAnchorTemp, KnotenlisteAnchorTemp2 : Knotenliste
      := KnotenlisteAnchor;
   begin
      Open (File, in_file, "graph.txt");
      --  Es folgen zwei while Schleifen die jede Zeile der
      --  graph.txt durchlaufen. Das erste Mal werden
      --  die Knoten festgelegt und beim zweiten Mal die
      --  Kanten zu jedem Knoten bestimmt.
      while not End_Of_File (File) loop
         Get_Line (File, AktuelleZeile);
         Zerlegt := Zerlege (AktuelleZeile);
         --  Es werden in dieser while Schleife nur die
         --  Knoten festgelegt. Es wird daher Source
         --  und Target als möglicher Knoten betrachtet.
         --  Nur wenn die aktuell betrachtete Knoten-Id
         --  noch nicht in der Knotenliste vorhanden ist
         --  wird ein neuer Knoten an die Liste angehängt.
         if In_Knotenliste (Zerlegt.Source) = False then
            AddToKnotenliste (Zerlegt.Source);
         end if;         
         if In_Knotenliste (Zerlegt.Target) = False then
            AddToKnotenliste (Zerlegt.Target);
         end if;
      end loop;
      --  Nach dem ersten Durchlaufen der Datei wird
      --  sie zurückgesetzt um für die folgende while
      --  Schleife neu zu beginnen.
      Reset (File);
      AktuelleZeile := Null_Unbounded_String;
      while not End_Of_File (File) loop
         Get_Line (File, AktuelleZeile);
         Zerlegt := Zerlege (AktuelleZeile);
         --  Es werden zwei Zeiger auf die Knotenliste
         --  gebraucht um erstens den Quellknoten
         --  und zweitens den Zielknoten festzuhalten.
         KnotenlisteAnchorTemp := KnotenlisteAnchor;
         KnotenlisteAnchorTemp2 := KnotenlisteAnchor;
         Abbruch := False;
         --  Die Schleife läuft solange bis der Zeiger
         --  KnotenlisteAnchorTemp am Ende der Liste angekommen ist.
         while Is_Empty (KnotenlisteAnchorTemp) /= True loop
            --  Wenn der aktuell betrachtete Knoten der gesuchte
            --  Quellknoten der Kante ist wird fortgefahren.
            if KnotenlisteAnchorTemp.Id = Zerlegt.Source then
               --  Die Schleife läuft solange bis der Zeiger
               --  KnotenlisteAnchorTemp2 am Ende der Liste angekommen ist.
               --  Es werden alle Elemente der Liste durchlaufen bis
               --  die gesuchte Knoten-Id gefunden wurde.             
               while Is_Empty (KnotenlisteAnchorTemp2) /= True loop
                  --  Wenn die aktuelle Knoten-Id dem gesuchten Zielknoten
                  --  entspricht ...
                  if KnotenlisteAnchorTemp2.Id = Zerlegt.Target then
                     --  ... wird ein neues Kanten-Element an die Liste
                     --  der Kanten des zuvor gesuchten Elements der
                     --  Knotenliste angehängt.
                     --  Das neue Element der Kantenliste wird
                     --  das zuvor ermittelte Gewicht und der Zielknoten
                     --  welcher in der obigen while Schleife gefunden wurde
                     --  zugeordnet.
                     --  falls zuvor leere Liste,
                     --  dann einzellige Liste erzeugen
                     if KnotenlisteAnchorTemp.Kanten = null then
                        KnotenlisteAnchorTemp.Kanten := new Kante'(null,
                        KnotenlisteAnchorTemp2, Zerlegt.W);
                        Abbruch := True;
                        exit;
                     else
                        --  Neues Element in die Kantenliste anfügen.
                        KnotenlisteAnchorTemp.Kanten :=
                        new Kante'(KnotenlisteAnchorTemp.Kanten,
                        KnotenlisteAnchorTemp2, Zerlegt.W);
                        Abbruch := True;
                        exit;
                     end if;
                  end if;
                  KnotenlisteAnchorTemp2 := KnotenlisteAnchorTemp2.Next;
               end loop;
            end if;
            if Abbruch = True then
               exit;
            end if;
            KnotenlisteAnchorTemp := KnotenlisteAnchorTemp.Next;
         end loop;
      end loop;
      Close (File);
   end Load;
   

   --  FUNCTION Get_Koord
   --
   --  Zur Ausgabe wird eine Matrix erzeugt. Beim
   --  Setzen der Gewichte in die Matrix wird
   --  eine horizontale Koordinate gebraucht.
   --
   --  PARAMETERS:
   --  + SearchUnb - Der gesuchte Knoten-Id dessen
   --  horizontale Koordinate ermittelt werden soll.
   --  RETURNS:
   --  Die Funktion liefert einen Integer-Wert
   --  zurück der die Position des gesuchten
   --  Knoten-Ids in der Knotenliste repräsentiert.
   function Get_Koord
      (SearchUnb : Unbounded_String)
      return Integer
   is
      KnotList : Knotenliste := KnotenlisteAnchor;
      ReturnMe : Integer := 0;
   begin
      while Is_Empty (KnotList) /= True loop 
         ReturnMe := ReturnMe + 1;
         if KnotList.Id = SearchUnb then
            return ReturnMe;            
         end if;
         KnotList := KnotList.Next;
      end loop;
      return 0;
   end Get_Koord;
   

   --  FUNCTION Fill_with_Spaces
   --
   --  Zur formatierten Ausgabe wurde oben die
   --  längste Id ermittelt. Diese Funktion
   --  füllt eine gegebene Zeichenkette solange
   --  mit Leerzeichen bis sie die selbe
   --  länge wie die längste Id hat. 
   --
   --  PARAMETERS:
   --  + FillMe - Die gegebene Zeichenkette
   --  dessen länge angeglichen werden soll.
   --  RETURNS:
   --  Die Funktion liefert einen Unbounded_String
   --  zurück mit angeglichener Länge, gefüllt
   --  mit Leerzeichen.
   function Fill_with_Spaces
      (FillMe : Unbounded_String)
      return Unbounded_String
   is
      ReturnMe : Unbounded_String := FillMe;
   begin
      while Length (ReturnMe) < (LaengsteId + 1) loop
         ReturnMe := " " & ReturnMe;
      end loop;
      return ReturnMe;
   end Fill_with_Spaces;


   --  PROCEDURE Destroy
   --
   --  Es wird der von allen Listen benötigte Speicher
   --  freigegeben.
   procedure Destroy is
      KnotenlisteAnchorx, KnotenlisteAnchorxCopy : Knotenliste
      := KnotenlisteAnchor;
      KantenlisteAnchorx, KantenlisteAnchorxCopy : Kantenliste := null;
   begin
      --  Die Schleife läuft für jedes Element der Knotenliste.
      while Is_Empty (KnotenlisteAnchorx) /= True loop
         KantenlisteAnchorx := KnotenlisteAnchorx.Kanten;
         --  Die Schleife läuft für jedes Element der Kantenliste
         --  des aktuellen Elements der Knotenliste.
         while KantenlisteAnchorx /= null loop
            KantenlisteAnchorxCopy := KantenlisteAnchorx;
            KantenlisteAnchorx := KantenlisteAnchorx.Next;
            FreeKante (KantenlisteAnchorxCopy);
         end loop;
         KnotenlisteAnchorxCopy := KnotenlisteAnchorx;
         KnotenlisteAnchorx := KnotenlisteAnchorx.Next;
         FreeKnoten (KnotenlisteAnchorxCopy);
      end loop;
      KnotenlisteAnchor := KnotenlisteAnchorx;
   end Destroy;


   --  PROCEDURE Reachable
   --
   --  Der Benutzer wird zur Eingabe einer Knoten-Id aufgefordert.
   --  Diese Knoten-Id identifiziert einen Start-Knoten.
   --  Von diesem Start-Knoten aus werden alle Knoten aufgelistet,
   --  die davon erreichbar sind.
   procedure Reachable is
      Eingabe : Unbounded_String := Null_Unbounded_String;
      --  Der BesuchtArray ist ein Array bestehend aus Boolean-feldern
      --  in der Groesse von 1 bis zur Anzahl von Elementen in der Knotenliste.
      --  Somit hat jede Knoten-Id ein entsprechendes Feld, das im folgenden
      --  gesetzt wird.
      BesuchtArray : array (1 .. KnotenlisteCounter) of Boolean
      := (others => False);


      --  PROCEDURE BesuchtArray_Ausgabe
      --
      --  Das von der rekursiven Prozedur Reku besetzte
      --  BesuchtArray wird parallel zur Knotenliste
      --  durchlaufen. Falls der aktuelle Knoten im Array
      --  besucht wurde, wird die Knoten-Id ausgegeben.
      procedure BesuchtArray_Ausgabe is
         KnotenlisteAnchorx : Knotenliste := KnotenlisteAnchor;
         Counter : Integer := 1;
         HatWas : Boolean := False;
      begin
         --  Alle Knoten der Knotenliste werden durchlaufen 
         --  und das entsprechende Boolean-Feld betrachtet.
         while Is_Empty (KnotenlisteAnchorx) /= True loop
            --  Wenn das Feld gesetzt wurde, wird die
            --  aktuelle Knoten-Id ausgegeben.
            if BesuchtArray (Counter) = True then
               if HatWas /= False then
                  Put (", " & To_String (KnotenlisteAnchorx.Id));
               else
                  Put ("Erreichbare Knoten: " &
                  To_String (KnotenlisteAnchorx.Id));
                  HatWas := True;
               end if;
            end if; 
            KnotenlisteAnchorx := KnotenlisteAnchorx.Next;
            Counter := Counter + 1;
         end loop;
         if HatWas = False then
            Put ("Es sind keine Knoten erreichbar.");
         end if;
         New_Line;
      end BesuchtArray_Ausgabe;


      --  PROCEDURE Reku
      --
      --  Die rekursive Prozedur Reku setzt die Boolean-Felder
      --  des BesuchtArrays. Es wird auf True gesetzt wenn
      --  der Knoten erreichbar ist.
      procedure Reku
         (IdUnb : in Unbounded_String)
      is
         KnotenlisteAnchorx : Knotenliste := KnotenlisteAnchor;
         KantenlisteAnchorx : Kantenliste := null;
      begin
         --  Nur wenn die gesuchte Knoten-Id in der Liste vorhanden
         --  ist wird fortgefahren.
         if In_Knotenliste (IdUnb) = True then
            --  Die Knotenliste wird durchlaufen und wenn der Läufer
            --  auf die gesuchte Knoten-Id trifft, wird fortgefahren.
            while Is_Empty (KnotenlisteAnchorx) /= True loop
               if KnotenlisteAnchorx.Id = IdUnb then
                  --  Nur wenn der Knoten Kanten besitzt, wird
                  --  weitergemacht.
                  if KnotenlisteAnchorx.Kanten /= null then
                     KantenlisteAnchorx := KnotenlisteAnchorx.Kanten;
                     --  Alle Kanten des Knotens werden durchlaufen.
                     while KantenlisteAnchorx /= null loop
                        --  Wenn die Knoten-Id des Kantenziels der aktuellen
                        --  Kante noch nicht besucht wurde, und somit das
                        --  Boolean Feld im BesuchtArray immernoch False ist,
                        --  wird fortgefahren.
                        if BesuchtArray (Get_Koord
                        (KantenlisteAnchorx.Kantenziel.Id)) = False then
                           --  Das entsprechende Boolean Feld des Knotens wird
                           --  auf Besucht gesetzt.
                           BesuchtArray (Get_Koord
                           (KantenlisteAnchorx.Kantenziel.Id)) := True;
                           --  Wenn alle Bedingungen bis zu dieser Stelle
                           --  erfüllt sind, wird rekursiv für die Knoten-Id
                           --  des Kantenziels der aktuellen Kante aufgerufen.
                           Reku (KantenlisteAnchorx.Kantenziel.Id);
                        end if;
                        KantenlisteAnchorx := KantenlisteAnchorx.Next;
                     end loop;
                  end if;
                  exit;
               end if;
               KnotenlisteAnchorx := KnotenlisteAnchorx.Next;
            end loop;
         else
            Put_Line ("Die Knoten-Id " & To_String (IdUnb) &
            " wurde nicht gefunden.");
         end if;
      end Reku;

   begin
      Put ("Bitte Startknoten eingeben: ");
      Get_Line (Eingabe);
      --  Die Boolean Felder des BesuchtArray werden ermittelt
      --  und gesetzt.
      Reku (Eingabe);
      --  Die gesetzten Daten werden sortiert ausgegeben.
      BesuchtArray_Ausgabe;
      --  Nachdem ausgegeben wurde, wird der benötigte Speicher freigegeben.
      Destroy;
   end Reachable;
   

   --  PROCEDURE Ausgabe
   --
   --  In dieser Prozedur wird eine Matrix in den Dimensionen,
   --  bestimmt durch die Anzahl der Elemente in der Knotenliste,
   --  definiert, mit den Kantengewichten gesetzt und formatiert
   --  ausgegeben. Es werden alle Felder der Matrix für den Anfang
   --  auf null gesetzt.
   procedure Ausgabe is
      Matrix : array (1 .. KnotenlisteCounter, 1 .. KnotenlisteCounter) of 
      Integer := (others => (others => 0));
      KnotenlisteAnchorx : Knotenliste := KnotenlisteAnchor;
      KantenlisteAnchorx : Kantenliste := null;
      VerticalCounter : Integer := 1;
   begin
      --  Es folgen zwei while Schleifen. In der Ersten
      --  wird durch alle Kanten jedes Knotens der
      --  Knotenliste gelaufen und die Kantengewichtige
      --  in die Matrix übertragen. In der Zweiten Schleife
      --  wird die Matrix formatiert ausgegeben.
      Put (Fill_with_Spaces (Null_Unbounded_String));
      while Is_Empty (KnotenlisteAnchorx) /= True loop
         Put (Fill_with_Spaces (KnotenlisteAnchorx.Id));
         KantenlisteAnchorx := KnotenlisteAnchorx.Kanten;
         while KantenlisteAnchorx /= null loop
            Matrix (Get_Koord (KantenlisteAnchorx.Kantenziel.Id),
            VerticalCounter) := KantenlisteAnchorx.Gewicht;
            KantenlisteAnchorx := KantenlisteAnchorx.Next;
         end loop;
         KnotenlisteAnchorx := KnotenlisteAnchorx.Next;
         VerticalCounter := VerticalCounter + 1;
      end loop;
      New_Line;
      KnotenlisteAnchorx := KnotenlisteAnchor;
      VerticalCounter := 1;
      while Is_Empty (KnotenlisteAnchorx) /= True loop
         Put (Fill_with_Spaces (KnotenlisteAnchorx.Id));
         for Laufvar in 1 .. KnotenlisteCounter loop
            Put (Fill_with_Spaces (To_Unbounded_String (Matrix (Laufvar,
            VerticalCounter)'Img)));
         end loop;
         New_Line;
         KnotenlisteAnchorx := KnotenlisteAnchorx.Next;
         VerticalCounter := VerticalCounter + 1;
      end loop;
      --  Nachdem ausgegeben wurde, wird der benötigte Speicher freigegeben.
      Destroy;
   end Ausgabe;

end graphen;
