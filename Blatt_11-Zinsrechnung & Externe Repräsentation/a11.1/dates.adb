--  FILE:    dates.adb
--
--  PROJECT: Programmieruebungen, Uebungsblatt 11
--  VERSION: $Revision: 275 $
--  DATE:    $Date: 2007-01-19 17:43:44 +0100 (Fri, 19 Jan 2007) $
--  AUTHOR:  $Author: keulsn $
--
-------------------------------------------------------------------------------


with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

package body Dates is


   function Create
     (Day   : in Integer;
      Month : in Integer;
      Year  : in Integer)
     return Date
   is
      Normalized_Day   : constant Integer := Day - 1;
      Normalized_Month : constant Integer := Month - 1;
      Normalized_Year  : constant Integer := Year;
   begin
      return Date
        ((Normalized_Year * 12 + Normalized_Month) * 30 + Normalized_Day);
   end Create;


   function Value
     (S : in String)
     return Date
   is
      use Ada.Strings.Fixed;
      First  : constant Natural := Index (S, ".");
      Second : constant Natural := Index (S (First + 1 .. S'Last), ".");
   begin
      if First = 0 or Second = 0 then
         raise Constraint_Error;
      end if;
      return Create (Integer'Value (S (S'First .. First - 1)),
                     Integer'Value (S (First + 1 .. Second - 1)),
                     Integer'Value (S (Second + 1 .. S'Last)));
   end Value;


   function Day
     (D : in Date)
     return Day_Range
   is
   begin
      return Day_Range (D mod 30 + Date'(1));
   end Day;


   function Month
     (D : in Date)
     return Month_Range
   is
   begin
      return Day_Range ((D / 30) mod 12 + Date'(1));
   end Month;


   function Year
     (D : in Date)
     return Year_Range
   is
   begin
      return Year_Range ((D / 30) / 12);
   end Year;


   function Image
     (D : in Date)
     return String
   is
      S : String (1 .. 10) := "XX.XX.XXXX";
   begin
      Ada.Integer_Text_IO.Put
        (To   => S (1 .. 2),
         Item => Day (D));
      Ada.Integer_Text_IO.Put
        (To   => S (4 .. 5),
         Item => Month (D));
      Ada.Integer_Text_IO.Put
        (To   => S (7 .. 10),
         Item => Year (D));
      for I in S'Range loop
         if S (I) = ' ' then
            S (I) := '0';
         end if;
      end loop;
      return S;
   end Image;


   function End_Of_Month
     (D : in Date)
     return Date
   is
   begin
      return Create (30, Month (D), Year (D));
   end End_Of_Month;


   function End_Of_Year
     (D : in Date)
     return Date
   is
   begin
      return Create (30, 12, Year (D));
   end End_Of_Year;


   function Earliest
     (Left  : in Date;
      Right : in Date)
     return Date
   is
   begin
      return Date'Min (Left, Right);
   end Earliest;


   function "+"
     (Left  : in Date;
      Days  : in Integer)
     return Date
   is
   begin
      return Date (Integer (Left) + Days);
   end "+";


   function "-"
     (Left  : in Date;
      Days  : in Integer)
     return Date
   is
   begin
      return Date (Integer (Left) - Days);
   end "-";


   function "-"
     (Left  : in Date;
      Right : in Date)
     return Integer
   is
   begin
      return Integer (Left) - Integer (Right);
   end "-";


   --  Implementation der Vergleichsoperatoren existiert aber ist nur
   --  im privaten Teil sichtbar => Redefinition notwendig.

   function "<"
     (Left  : in Date;
      Right : in Date)
     return Boolean
   is
   begin
      return Integer (Left) < Integer (Right);
   end "<";


   function "<="
     (Left  : in Date;
      Right : in Date)
     return Boolean
   is
   begin
      return Integer (Left) <= Integer (Right);
   end "<=";


   function ">"
     (Left  : in Date;
      Right : in Date)
     return Boolean
   is
   begin
      return Integer (Left) > Integer (Right);
   end ">";


   function ">="
     (Left  : in Date;
      Right : in Date)
     return Boolean
   is
   begin
      return Integer (Left) >= Integer (Right);
   end ">=";


end Dates;
