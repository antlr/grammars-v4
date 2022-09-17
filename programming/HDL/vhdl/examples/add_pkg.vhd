library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package add_pkg is

  procedure add_unsigned (
    signal a : in  std_logic_vector;
    signal b : in  std_logic_vector;
    signal z : out std_logic_vector
    );

end add_pkg;

package body add_pkg is

  procedure add_unsigned (
    signal a : in  std_logic_vector;
    signal b : in  std_logic_vector;
    signal z : out std_logic_vector
    )
  is
  begin
    z <= std_logic_vector(unsigned(a) + unsigned(b));
  end add_unsigned;
  
end add_pkg;
