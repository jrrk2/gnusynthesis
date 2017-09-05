-- This file is part of the ethernet_mac project.
--
-- For the full copyright and license information, please read the
-- LICENSE.md file that was distributed with this source code.

-- MAC sublayer functionality (en-/decapsulation, FCS, IPG)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity other is

  port(
		temp                : out  std_ulogic;
		clock_i             : in  std_ulogic;
        tx_fcs_o            : out std_logic_vector(2 downto 0)
	);
end entity;

architecture rtl of other is

  signal dummy : std_logic_vector(3 downto 0);
  
begin
    tx_fcs_o <= ( others => '1' );
    temp <= dummy(0);
    
end architecture;
