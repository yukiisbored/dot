local theme_assets = require("beautiful.theme_assets")
local dpi          = require("beautiful.xresources").apply_dpi

local theme = {}

theme.dir                   = os.getenv("HOME") .. "/.config/awesome/themes/yuki"

theme.wallpaper             = theme.dir .. "/wallpaper.jpg"
theme.font                  = "Noto Sans 8"

theme.bg_normal             = "#111111dd"
theme.bg_focus              = "#444444dd"
theme.bg_urgent             = "#ff0000"
theme.bg_minimize           = "#555555"
theme.bg_systray            = theme.bg_normal

theme.fg_normal             = "#ffffff"
theme.fg_focus              = "#ffffff"
theme.fg_urgent             = "#ffffff"
theme.fg_minimize           = "#ffffff"

theme.useless_gap           = dpi(8)
theme.border_width          = dpi(4)

theme.border_normal         = "#000000"
theme.border_focus          = "#000000"
theme.border_marked         = "#ff0000"

theme.menu_height           = dpi(15)
theme.menu_width            = dpi(100)

theme.taglist_square_size   = dpi(4)

theme.taglist_squares_sel   = theme_assets.taglist_squares_sel(theme.taglist_square_size, theme.fg_normal)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(theme.taglist_square_size, theme.fg_normal)

return theme
