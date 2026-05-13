-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices
config.ssh_domains = {
  {
    -- The name of this specific domain.  Must be unique amongst
    -- all types of domain in the configuration file.
    name = 'my.ssh_pg8',
    remote_address = 'pg08.phys.lsu.edu',
    remote_wezterm_path = "/home/jpmendez/wezterm",
    username = "jpmendez"
  },
}
-- config.font = wezterm.font 'Fira Code'
-- You can specify some parameters to influence the font selection;
-- for example, this selects a Bold, Italic font variant.
-- config.font =
  wezterm.font('JetBrains Mono', { weight = 'Bold', italic = true })
  
-- How many lines of scrollback you want to retain per tab
config.scrollback_lines = 3500
config.enable_tab_bar = false
--- config.color_scheme = 'Windows 95 Light (base16)'
config.color_scheme = 'Ivory Light (terminal.sexy)'
-- and finally, return the configuration to wezterm
return config
