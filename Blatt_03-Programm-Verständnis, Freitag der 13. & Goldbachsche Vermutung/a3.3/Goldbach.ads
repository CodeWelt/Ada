--  FILE:    Goldbach.ads
--
--  PROJECT: Programmieruebungen, Uebungsblatt 3
--  VERSION: 1.0
--  DATE:    17.11.2006
--  AUTHOR:  http://CodeWelt.com
--
-------------------------------------------------------------------
--
--  Aufgabe 3.3: Goldbachsche Vermutung
--
--  Die Vermutung ist dass jede gerade Zahl größer als 2 als
--  Summe zweier Primzahlen geschrieben werden kann.
--  Das Programm überprüft ob diese Vermutung in einem
--  bestimmten Bereich zutrifft oder nicht.
--  Das Package Goldbach enthält zwei Prozeduren und die typen
--  Prime_Number und Prime_Field.
--
-------------------------------------------------------------------

package Goldbach is

   subtype Prime_Number is Integer range 2 .. Positive'Last;
   
   --  TYPE Prime_Field
   --
   --  Der Algorithmus Sieb des Eratosthenes wird verwendet
   --  um die Komponenten dieses Felds Prime_Field an jeder
   --  Index-Position, die eine Primzahl ist, den Wert True
   --  zu geben, an allen anderen den Wert False.
   --  Das Feld wird vom Programm GC_Prover, nach der Eingabe
   --  des Benutzers, in geeigneter größe erstellt.
   type Prime_Field is array (Positive range <>) of Boolean;

   --  PROCEDURE Eratosthenes
   --  Die Prozedur Eratosthenes wird aufgerufen, um die Komponenten
   --  des als Parameter übergebenen Felds Sieve sinnvoll zu belegen.
   --  Die Komponenten sollen an jeder Index-Position, die eine
   --  Primzahl ist, den Wert True enthalten, an allen anderen den
   --  Wert False. Die Prozedur verwendet den Algorithmus
   --  Sieb des Eratosthenes.
   --
   --  PARAMETERS:
   --  Sieve: Dieses Feld soll an jeder Index-Position, die eine
   --  Primzahl ist, den Wert True enthalten, an allen anderen
   --  den Wert False. Am Anfang vom Programm GC_Prover wurden alle
   --  Komponenten des Felds auf True gesetzt.
   procedure Eratosthenes
      (Sieve : in out Prime_Field);

   --  PROCEDURE Prove_Conjecture
   --  Die Prozedur wird für alle relevanten geraden Zahlen
   --  aufgerufen. Die Prozedur nimmt als Eingabe Number eine
   --  gerade Zahl und das zuvor berechnete Feld Is_Prime.
   --  Es wird der Ausgabe-Parameter Found auf True gesetzt,
   --  falls eine Zerlegung der geraden Zahl in die Summe
   --  zweier Primzahlen existiert, andernfalls auf False.
   --  Falls die zwei Primzahlen existieren, so speichert die
   --  Prozedur diese in den Ausgabe-Parametern First und Second.
   --
   --  PARAMETERS:
   --  Number: Dies ist eine gerade Zahl die in zwei Primzahlen
   --  zerlegt werden soll.
   --  Is_Prime: Dies ist das zuvor berechnete Feld das an jeder
   --  Index-Position, die eine Primzahl ist, den Wert True enthält.
   procedure Prove_Conjecture
      (Number : in Positive;
       Is_Prime : in Prime_Field;
       First : out Prime_Number;
       Second : out Prime_Number;
       Found : out Boolean);

end Goldbach;
