-- Ada 2022: Formal type defaults with OR USE (RM 12.5)
generic
   type Element_Type is private or use Integer;
   type Index_Type is (<>) or use Natural;
package Formal_Defaults is
   pragma Elaborate_Body;
   type Container is array (Index_Type range <>) of Element_Type;
end Formal_Defaults;
