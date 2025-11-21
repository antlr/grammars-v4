library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


package test_pkg is

  function with_params_f(data : std_logic_vector(31 downto 0)) return std_logic_vector;

  function without_params_f return natural;

  procedure no_params_p;

  procedure nested_outer_proc(signal clk_i : in std_logic);

  function nested_outer_func return integer;

  component component_declaration_without_is
    port(
      clk_i : in std_logic
    );
  end component;

  component component_declaration_with_is is
    port(
      clk_i : in std_logic
    );
  end component;

  component component_without_generics_or_ports is
  end component;

  component component_with_trailing_label is
    port(
      clk_i : in std_logic
    );
  end component component_with_trailing_label;

end package;



package body test_pkg is

  procedure body_defined_p;

  procedure body_defined_p is begin
    wait for 5.0e-9 * 1 sec;
  end procedure;

  function with_params_f(data : std_logic_vector(31 downto 0)) return std_logic_vector is
    begin
  end function;

  function without_params_f return natural is begin
    return 0;
  end function;

  procedure no_params_p is begin
  end procedure;

  procedure nested_outer_proc(signal clk_i : in std_logic) is
    procedure nested_inner_proc(signal clk_i : in std_logic) is begin
      wait until rising_edge(clk_i);
    end procedure;
  begin
    nested_inner_proc(clk_i);
  end;

  function nested_outer_func return natural is
    function nested_inner_func return natural is begin
      return 0;
    end function;
  begin
    return nested_inner_func;
  end function;

end test_pkg;