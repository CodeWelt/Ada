--  FILE:    Expression_Trees.adb
--  PROJECT: Programmieruebungen, Uebungsblatt 9
--  VERSION: 1.0
--  DATE:    13.01.2007
--  AUTHOR: http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 9.3: Baumtraversierung
--
--  Das Package bietet Funktionalität für z.B. das Einlesen eines
--  Ausdrucks in UPN und Ausgabe in Preoder und Inorder.
--
-------------------------------------------------------------------
with Node_Stacks, Ada.Text_IO, Ada.Strings.Unbounded,
     Ada.Strings, Ada.Characters.Handling,
     Ada.Unchecked_Deallocation, Ada.Float_Text_IO;
use  Node_Stacks, Ada.Text_Io, Ada.Strings.Unbounded,
     Ada.Strings;

package body Expression_Trees is

   FinalValue : Float := 0.00;

   procedure Free is new Ada.Unchecked_Deallocation(Node, Expression_Tree);
    

   --  PROCEDURE Parse_Postorder
   --
   --  Die Prozedur Parse_Postorder erstellt aus einem
   --  gegebenen String einen Baum aus Knoten und Blättern.
   --  Es wird Zeichen für Zeichen der übergebenen
   --  Expression durchgegangen. Aus Operationen werden
   --  Knoten deren linke und rechte verzweigung entweder
   --  ein Blatt, also ein Wert als float enthält, oder
   --  einen weiteren knoten.
   --  
   --  PARAMETERS:
   --  + Expression als String. Dieser String enthält die
   --  z.B. vom Benützer eingegebenen Ausdruck in UPN.
   --  Dieser String wird analysiert.
   --  Tree - Der Zeiger auf den Baum als Expression_Tree
   procedure Parse_Postorder
      (Expression : in     String;
      Tree       :    out Expression_Tree)
   is
      StackPointer : Ref_Stack;      
      Operation : Expression_Tree;
      V1Left : Expression_Tree;
      V2Right : Expression_Tree;
      Fragment : Unbounded_String := Null_Unbounded_String;
      ExpressionUnb : Unbounded_String := Null_Unbounded_String;
      
      FragmentFill : Boolean := False;
      PushThis : Expression_Tree;
   begin      
      ExpressionUnb := To_Unbounded_String (Expression);

      if Length(ExpressionUnb) > 0 then
         --  Die Schleife läuft solange bis alle Zeichen der übergebenen
         --  Expression gelesen wurden.
         while ExpressionUnb /= Null_Unbounded_String loop
            --  Wenn das aktuelle Zeichen ein Leerzeichen ist:
            if Element (ExpressionUnb, 1) = ' ' then
               --  Wenn z.B. eine mehrstellige Float Zahl wie 3.4 angegeben wurde,
               --  die unten in das Fragment eingelesen wird, wird das Fragment
               --  geschlossen sobald ein Leerzeichen eingelesen wird.
               if FragmentFill = True then
                  --  Das gesamte Fragment wird als Float auf den Stack gesetzt.
                  PushThis := new Node'('=', Float'Value (To_String (Fragment)));
                  Push (StackPointer, PushThis);
                  FinalValue := Float'Value (To_String (Fragment));
                  --  Es wird alles zurückgesetzt um das nächste Fragment einlesen
                  --  zu können.           
                  FragmentFill := False;
                  Delete (ExpressionUnb, 1, 1);
                  Fragment := Null_Unbounded_String;
               else
                  --  Andernfalls wird das Leerzeichen übergangen.
                  Delete (ExpressionUnb, 1, 1);
               end if;
                  
            --  Wenn das aktuelle Zeichen eine Operation darstellt:   
            elsif Element (ExpressionUnb, 1) = '+' or Element (ExpressionUnb, 1) = '-'
            or Element (ExpressionUnb, 1) = '*' or Element (ExpressionUnb, 1) = '/' then
               if FragmentFill = True then
                  raise Malformed_Expression;
               end if;
               --  Werden zwei Elemente vom Stack genommen und jeweils als linker oder
               --  rechter Operand der Operation gesetzt.
               V1Left := Top (StackPointer);
               StackPointer := Pop (StackPointer, False);
               V2Right := Top (StackPointer);
               StackPointer := Pop (StackPointer, False);   
            
               --  value for discriminant "Kind" must be static
               if Element (ExpressionUnb, 1) = '+' then
                  Operation := new Node'('+', V1Left, V2Right);
               elsif Element (ExpressionUnb, 1) = '-' then
                  Operation := new Node'('-', V1Left, V2Right);
               elsif Element (ExpressionUnb, 1) = '*' then
                  Operation := new Node'('*', V1Left, V2Right);
               elsif Element (ExpressionUnb, 1) = '/' then
                  Operation := new Node'('/', V1Left, V2Right);
               end if;
               
               --  Der Zeiger des neu erstellten Baums wird auf den stack
               --  geschoben.
               Push (StackPointer, Operation);
               Delete (ExpressionUnb, 1, 1);
            --  Wenn da aktuelle Zeichen ein Teil einer Zahl darstellt:
            elsif Element (ExpressionUnb, 1) = '.' or
            Ada.Characters.Handling.Is_Digit (Element (ExpressionUnb, 1)) = True then
               --  Wird das Zeichen an die bisherigen Teile der mehrstelligen
               --  Zahl angehänt. Wenn als nächstes ein Leerzeichen folgt, wird
               --  das Einlesen der Zahl beendet und der Wert ermittelt.
               Fragment := Fragment & Element (ExpressionUnb, 1);
               FragmentFill := True;
               Delete (ExpressionUnb, 1, 1);
            else
               Put_Line ("Die Expression enthält Zeichen die nicht erlaubt sind.");
               Put_Line ("Das Zeichen " & Element (ExpressionUnb, 1) & " wird übergangen.");
               Delete (ExpressionUnb, 1, 1);
            end if;

         end loop;

         Push (StackPointer, Operation);
         Tree := Top (StackPointer);

      else
         Put_Line ("Die angegebene Expression enthält keine Zeichen.");
      end if;

   end Parse_Postorder;


   --  FUNCTION Is_Empty
   --
   --  Die Funktion Is_Empty ermittelt ob der Baum leer
   --  ist oder nicht.
   --
   --  PARAMETERS:
   --  + Tree - Der Zeiger auf den Baum als Expression_Tree
   --  RETURNS:
   --  Die Funktion liefert True zurück wenn der Baum leer
   --  ist, andernfalls False.
   function Is_Empty
     (Tree : in Expression_Tree)
     return Boolean
   is
      EmptyTree : Expression_Tree;
   begin
      return Tree = EmptyTree;
   end Is_Empty;


   --  FUNCTION Value
   --
   --  Die Funktion Value ermittelt den Wert des Baums.
   --  Es wird ähnlich wie bei Put_Inorder vorgegangen.
   --  Zu jedem Node wird rekursiv das linke und rechte
   --  Blatt berechnet.
   --
   --  PARAMETERS:
   --  + Tree - Der Zeiger auf den Baum als Expression_Tree 
   --  RETURNS:
   --  Die Funktion liefert das Ergebnis des Baums als
   --  Float wert zurück.
   function Value
     (Tree : in Expression_Tree)
     return Float
   is
      ReturnMe, Operation, Wert : Float := 0.00;
      RunOnce : Boolean := True;


      --  PROCEDURE Calculate
      --
      --  Die Prozedur Calculate nimmt die zwei
      --  von der Prozedur Reku gesetzten Werte
      --  Operation, welche die Art der Operation
      --  representiert, und Wert, die eigentliche
      --  Zahl welche z.B. addiert werden soll und
      --  führt die Operation aus.
      procedure Calculate is
      begin
         if Operation = 1.00 then
            ReturnMe := ReturnMe - Wert;
         elsif Operation = 2.00 then
            ReturnMe := ReturnMe + Wert;

         elsif Operation = 3.00 then
            ReturnMe := ReturnMe * Wert;

         elsif Operation = 4.00 then      
            ReturnMe := ReturnMe / Wert;
         end if;
         
      end Calculate;


      --  PROCEDURE Reku
      --
      --  Die rekursive Prozedur Reku läuft durch alle
      --  Elemente des gegebenen Baums. Mit hilfe der
      --  Case unterscheidung wird ermittelt ob das aktuelle
      --  Element des Baums ein Knoten, also eine Operation
      --  darstellt, oder ein Blatt, welches immer einen
      --  Float Wert enthält, ist.
      --  Wenn das Element ein Knoten ist, wird die Prozedur
      --  Reku rekursiv für die linke und rechte Verzweigung
      --  aufgerufen.
      --  Die Abbruchbedingung ist immer dann aktiv, wenn
      --  die Rekursion auf ein Blatt trifft. Dann wird die
      --  Rekursion beendet und der Wert ausgegeben.
      --  
      --  PARAMETERS:
      --  + Tree - Der Zeiger auf den Baum als Expression_Tree
      procedure Reku
         (Tree : in Expression_Tree)
      is
      begin
         if Is_Empty (Tree) /= True then
            case Tree.Kind is
               when '-' | '+' | '*' | '/' =>
                  Reku(Tree.Right);
                  if Tree.Kind = '-' then
                     Operation := 1.00;
                  elsif Tree.Kind = '+' then
                     Operation := 2.00;
                  elsif Tree.Kind = '*' then
                     Operation := 3.00;
                  elsif Tree.Kind = '/' then
                     Operation := 4.00;
                  end if;
                  Reku(Tree.Left);
               when others =>
                  if RunOnce = True then
                     ReturnMe := Tree.Value;
                     RunOnce := False;
                  else
                     Wert := Tree.Value;
                     if Operation /= 0.00 then
                        Calculate;
                     end if;
                  end if;    
            end case;
         else
            Put_Line ("Der Baum ist leer.");
            raise Empty_Tree;
         end if;
      end Reku;

   begin
      Reku (Tree);
      return ReturnMe;
   end Value;


   --  PROCEDURE Put_Preorder
   --
   --  Die Prozedur Put_Preorder gibt den parsed Baum
   --  in Preorder aus.
   --  
   --  PARAMETERS:
   --  + Tree - Der Zeiger auf den Baum als Expression_Tree
   procedure Put_Preorder
     (Tree : in Expression_Tree)
   is
      RunOnce : Boolean := True;


      --  PROCEDURE Reku
      --
      --  Die rekursive Prozedur Reku läuft durch alle
      --  Elemente des gegebenen Baums. Mit hilfe der
      --  Case unterscheidung wird ermittelt ob das aktuelle
      --  Element des Baums ein Knoten, also eine Operation
      --  darstellt, oder ein Blatt, welches immer einen
      --  Float Wert enthält, ist.
      --  Wenn das Element ein Knoten ist, wird die Prozedur
      --  Reku rekursiv für die linke und rechte Verzweigung
      --  aufgerufen.
      --  Die Abbruchbedingung ist immer dann aktiv, wenn
      --  die Rekursion auf ein Blatt trifft. Dann wird die
      --  Rekursion beendet und der Wert ausgegeben.
      --  
      --  PARAMETERS:
      --  + Tree - Der Zeiger auf den Baum als Expression_Tree
      procedure Reku
         (Tree : in Expression_Tree)
      is
      begin
         
      
         if Is_Empty(Tree) /= True then
            --  Mit hilfe dieser Case unterscheidung
            --  wird ermittelt ob das aktuelle Element des Baums
            --  ein Knoten oder ein Blatt ist.
            case Tree.Kind is
               --  Wenn das Element einen Knoten, also eine Operation,
               --  darstellt, wird die Prozedur Reku rekursiv
               --  für die linke und rechte Verzweigung aufgerufen.
               when '-' | '+' | '*' | '/' =>
                  
                  Put ("""" & Tree.Kind & """ ");
                  Put (" (");
                  Reku(Tree.Right);
                  Reku(Tree.Left);
               --  Abbruchbedingung: Wenn die Rekursion auf ein Blatt
               --  trifft, wird die Rekursion beendet und der Wert
               --  ausgegeben.   
               when others =>
                  Ada.Float_Text_IO.Put (Tree.Value, 0, 5, 0);
                  if RunOnce = True then
                     Put (", ");
                     RunOnce := False;
                  elsif FinalValue = Tree.Value then
                     Put (")");
                  else
                     Put ("), ");
                  end if;
            end case;
         else
            Put_Line ("Der Baum ist leer.");
            raise Empty_Tree;
         end if;

      end Reku;



   begin
      Put ("Preorder: ");
      Reku (Tree);
      Put (" = ");
      Ada.Float_Text_IO.Put (Value (Tree), 0, 5, 0);
      New_Line;
   end Put_Preorder;


   --  PROCEDURE Put_Preorder
   --
   --  Die Prozedur Put_Preorder gibt den parsed Baum
   --  in Inorder aus.
   --  
   --  PARAMETERS:
   --  + Tree - Der Zeiger auf den Baum als Expression_Tree
   procedure Put_Inorder
     (Tree : in Expression_Tree)
   is
      

      --  PROCEDURE Reku
      --
      --  Die rekursive Prozedur Reku läuft durch alle
      --  Elemente des gegebenen Baums. Mit hilfe der
      --  Case unterscheidung wird ermittelt ob das aktuelle
      --  Element des Baums ein Knoten, also eine Operation
      --  darstellt, oder ein Blatt, welches immer einen
      --  Float Wert enthält, ist.
      --  Wenn das Element ein Knoten ist, wird die Prozedur
      --  Reku rekursiv für die linke und rechte Verzweigung
      --  aufgerufen.
      --  Die Abbruchbedingung ist immer dann aktiv, wenn
      --  die Rekursion auf ein Blatt trifft. Dann wird die
      --  Rekursion beendet und der Wert ausgegeben.
      --  
      --  PARAMETERS:
      --  + Tree - Der Zeiger auf den Baum als Expression_Tree
      --  + Operationx - Dieser Parameter übergibt die Operation
      --  der letzten Rekursionsstufe um zu überprüfen ob
      --  eine Klammerung notwendig ist.
      procedure Reku
         (Tree : in Expression_Tree;
          Operationx : in Character)
      is
      begin
         if Is_Empty(Tree) /= True then
            --  Mit hilfe dieser Case unterscheidung
            --  wird ermittelt ob das aktuelle Element des Baums
            --  ein Knoten oder ein Blatt ist.
            case Tree.Kind is
               --  Wenn das Element einen Knoten, also eine Operation,
               --  darstellt, wird die Prozedur Reku rekursiv
               --  für die linke und rechte Verzweigung aufgerufen.
               when '-' | '+' | '*' | '/' =>
                  --  Wenn die jetzige Operation eine "Strich-Operation"
                  --  ist und die letzte Operation, die als Parameter
                  --  übergeben wurde, eine Punkt-Operation war,
                  --  wird eine Klammer-auf gesetzt.
                  if Tree.Kind = '-' or Tree.Kind = '+' then
                     if Operationx = '*' or Operationx = '/' then
                        Put("(");
                     end if;
                  end if;
                  Reku (Tree.Right, Tree.Kind);
                  Put (" " & Tree.Kind & " ");

                  Reku (Tree.Left, Tree.Kind);
                  --  Wenn die jetzige Operation eine "Strich-Operation"
                  --  ist und die letzte Operation, die als Parameter
                  --  übergeben wurde, eine Punkt-Operation war,
                  --  wird eine Klammer-zu gesetzt.                  
                  if Tree.Kind = '-' or Tree.Kind = '+' then
                     if Operationx = '*' or Operationx = '/' then
                        Put(")");
                     end if;
                  end if;
               --  Abbruchbedingung: Wenn die Rekursion auf ein Blatt
               --  trifft, wird die Rekursion beendet und der Wert
               --  ausgegeben.                        
               when others =>
                  Ada.Float_Text_IO.Put (Tree.Value, 0, 5, 0);
            end case;
         else
            Put_Line ("Der Baum ist leer.");
            raise Empty_Tree;
         end if;
      end Reku;
      
   begin
      Put ("Inorder: ");
      Reku (Tree, ' ');
      Put (" = ");
      Ada.Float_Text_IO.Put (Value (Tree), 0, 5, 0);
      New_Line;
     
   end Put_Inorder;


   --  PROCEDURE Destroy
   --
   --  Die rekursive Prozedur Destroy befreit den Speicher
   --  vom Inhalt des Baums.
   --  
   --  PARAMETERS:
   --  + Tree - Der Zeiger auf den Baum als Expression_Tree
   --  von welchem der Speicher befreit werden soll.
   procedure Destroy
     (Tree : in out Expression_Tree)
   is
      EmptyTree : Expression_Tree;
   begin
      if Is_Empty(Tree) /= True then
         --  Mit hilfe dieser Case unterscheidung
         --  wird ermittelt ob das aktuelle Element des Baums
         --  ein Knoten oder ein Blatt ist.
         case Tree.Kind is
            --  Wenn das Element einen Knoten, also eine Operation,
            --  darstellt, wird die Prozedur Destroy rekursiv
            --  für die linke und rechte Verzweigung aufgerufen.
            --  Nachdem die Verzweigung auf ein Blatt trifft,
            --  wird das Blatt befreit und danach der ganze Knoten.
            when '-' | '+' | '*' | '/' =>
               Destroy (Tree.Right);
               Free (Tree.Right);
               Destroy (Tree.Left);
               Free (Tree.Left);
            when others =>
               Free (Tree); 
         end case;
      else
         Put_Line ("Der Baum ist leer.");
         raise Empty_Tree;
      end if;
      Tree := EmptyTree;
   end Destroy;

end Expression_Trees;
