library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity generic_mux is
  generic (
    type DATA_TYPE
  );
  port (
    sel : in  std_logic;
    a_i : in  DATA_TYPE;
    b_i : in  DATA_TYPE;
    z_o : out DATA_TYPE
  );
end entity;

architecture rtl of generic_mux is

  signal a : DATA_TYPE;
  signal b : DATA_TYPE;

begin

  a <= a_i;
  b <= b_i;

  process(all) begin
    if sel = '1' then
      z_o <= b;
    else
      z_o <= a;
    end if;
  end process;
end architecture;