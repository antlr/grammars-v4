library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package literals is

  constant l00 : natural                      := 16#12#;
  constant l01 : real                         := 1.0;
  constant l02 : real                         := 2e3;
  constant l03 : real                         := 4.0e5;
  constant l04 : real                         := -5.0e6;
  constant l05 : std_logic_vector(7 downto 0) := x"05";
  constant l06 : std_logic_vector(5 downto 0) := o"06";
  constant l07 : std_logic_vector(1 downto 0) := b"01";

  type t00 is range -1 to 1;

  constant l08 : string := "This is a simple string";
  constant l09 : string := "This is ' a complicated \n string!";

end package;