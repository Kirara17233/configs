import Xmobar

cursorFgColor = "#0f111a"
cursorBgColor = "#ffcc00"
foregroundColor = "#8f93a2"
backgroundColor = "#0f111a"
highlightFgColor = "#8f93a2"
highlightBgColor = "#1f2233"
black = "#546e7a"
red = "#ff5370"
green = "#c3e88d"
yellow = "#ffcb6b"
blue = "#82aaff"
purple = "#c792ea"
cyan = "#89ddff"
white = "#ffffff"
lightBlack = "#546e7a"
lightRed = "#ff5370"
lightGreen = "#c3e88d"
lightYellow = "#ffcb6b"
lightBlue = "#82aaff"
lightPurple = "#c792ea"
lightCyan = "#89ddff"
lightWhite = "#ffffff"

main :: IO ()
main = xmobar defaultConfig
  { font = "xft:MesloLGS NF:style=Regular:pixelsize=13"
  , additionalFonts =
      [ "xft:Symbols Nerd Font:style=2048-em:pixelsize=15"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=30" ]
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
      [ Run $ UnsafeStdinReader
      , Run $ Com "echo" ["<fn=3>\xf17c</fn>"] "penguin" 3600
      , Run $ Com "uname" ["-r"] "kernel" 3600
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
  , template = "<fn=2><fc="++white++">\xe61f</fc></fn><fc=#ffffff>|</fc> %UnsafeStdinReader%}{<box type=Bottom width=2 mb=2 color="++blue++"><fc="++blue++"><fn=1>\xf303</fn> <action=`termonad -e s`>%kernel%</action></fc></box>"
}
