-- Ada 2022: Formal type with OR USE default (RM 12.5)
generic
   type T is private or use Integer;
procedure Use_Formal_Incomplete (X : T);
