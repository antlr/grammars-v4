library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;


entity test_generate is
  generic (
    GEN_CONDITION : boolean := FALSE
  );
  port (
    clk_i  : in  std_logic;
    data_i : in  std_logic_vector(31 downto 0);
    data_o : out std_logic_vector(31 downto 0)
  );
end entity ;


architecture rtl of test_generate is
  
  constant TEST_CONSTANT : natural := 2;
  signal   data          : std_logic_vector(31 downto 0) := (others => '0');

begin

  process(clk_i) begin
    if rising_edge(clk_i) then
      data_o <= std_logic_vector(signed(data_i) + 1);
    end if;
  end process;


  GEN_LOOP : for i in 0 to 3 generate

    component component_declaration_in_generate_loop is
      port(
        clk_i : in std_logic
      );
    end component;
    
    signal test_signal : integer := 0;
    
  begin

    process(all) begin
    end process;

    process(clk_i) begin
      if rising_edge(clk_i) then
      end if;
    end process;

    LABELED_PROC : process(clk_i) begin
      if rising_edge(clk_i) then
      end if;
    end process LABELED_PROC;

  end generate;

  GEN_CONDITIONAL : if TRUE generate

    signal nested_signal_1 : std_logic_vector(31 downto 0) := (others => '0');

  begin

    process(clk_i) begin
      if rising_edge(clk_i) then
        nested_signal_1 <= data_i;
      end if;
    end process;


    GEN_NESTED : if TRUE generate

      signal nested_signal_2 : std_logic_vector(31 downto 0) := (others => '0');

    begin

      process(clk_i) begin
        if rising_edge(clk_i) then
          nested_signal_2 <= nested_signal_1;
        end if;
      end process;

    end generate GEN_NESTED;

  end generate;

  GEN_ELSIF : if GEN_CONDITION generate
    data <= data_i;

  elsif GEN_CONDITION generate

    process(clk_i) begin
      if rising_edge(clk_i) then
        data <= data_i;
      end if;
    end process;

  else generate

  end generate;

  GEN_CASE : case TEST_CONSTANT generate

    when 0 =>
      signal anded : std_logic;
    begin
      anded <= and(data_i);

    when 1 =>
      signal ored : std_logic;
    begin
      ored <= or(data_i);

    when 2 =>
      signal xored : std_logic;
    begin
      xored <= xor(data_i);

    when 3 | 4 =>
      signal xnored : std_logic;
    begin
      xnored <= xnor(data_i);

    when others =>

  end generate GEN_CASE;

  process(all) begin
    for i in 0 to 3 loop

    end loop;
  end process;

end architecture;