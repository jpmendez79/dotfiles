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
config.window_background_opacity = 0.60
config.enable_tab_bar = false
-- and finally, return the configuration to wezterm
return config