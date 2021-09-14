import Xmobar

cursorFgColor = "#0f111a"
cursorBgColor = "#ffcc00"
foregroundColor = "#8f93a2"
backgroundColor = "#0f111a"
highlightFgColor = "#8f93a2"
highlightBgColor = "#1f2233"
color0 = "#546e7a"
color1 = "#ff5370"
color2 = "#c3e88d"
color3 = "#ffcb6b"
color4 = "#82aaff"
color5 = "#c792ea"
color6 = "#89ddff"
color7 = "#ffffff"
color8 = "#546e7a"
color9 = "#ff5370"
color10 = "#c3e88d"
color11 = "#ffcb6b"
color12 = "#82aaff"
color13 = "#c792ea"
color14 = "#89ddff"
color15 = "#ffffff"

main :: IO ()
main = xmobar defaultConfig
  { font = "xft:MesloLGS NF:style=Regular:pixelsize=13"
  , additionalFonts = []
  , bgColor = backgroundColor
  , fgColor = foregroundColor
  , position = TopSize L 50 21
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = False
  , hideOnStart = False
  , iconRoot = "/etc/config/icons"
  , allDesktops = True
  , commands = 
      [ Run $ Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
      , Run $ Com ".local/bin/kernel" [] "kernel" 36000
      , Run $ Cpu ["-t", "<fn=2>\xf108</fn>  cpu: (<total>%)","-H","50","--high","red"] 20
      , Run $ Memory ["-t", "<fn=2>\xf233</fn>  mem: <used>M (<usedratio>%)"] 20
      , Run $ DiskU [("/", "<fn=2>\xf0c7</fn>  hdd: <free> free")] [] 60
      , Run $ Com "echo" ["<fn=2>\xf0aa</fn>"] "uparrow" 3600
      , Run $ Uptime ["-t", "uptime: <days>d <hours>h"] 360
      , Run $ Com "echo" ["<fn=2>\xf0f3</fn>"] "bell" 3600
      , Run $ Com ".local/bin/pacupdate" [] "pacupdate" 36000
      , Run $ Date "<fn=2>\xf017</fn>  %b %d %Y - (%H:%M) " "date" 50
      , Run $ Com "/home/dt/.config/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
      , Run $ UnsafeStdinReader ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = " <icon=Arch.xpm/> <fc=#ffffff>|</fc>"
}

