import Xmobar

main :: IO ()
main = xmobar defaultConfig
  { font = "xft:MesloLGS NF:style=Regular:pixelsize=10"
  , additionalFonts = []
  , bgColor = "black"
  , fgColor = "grey"
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
  , template = " <icon=Haskell-White.xpm/> "
}

