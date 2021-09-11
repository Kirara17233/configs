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
  , template = " <icon=haskell_20.xpm/>   <fc=#666666>|</fc> %UnsafeStdinReader% }{ <box type=Bottom width=2 mb=2 color=#51afef><fc=#51afef>%penguin%  <action=`alacritty -e htop`>%kernel%</action> </fc></box>    <box type=Bottom width=2 mb=2 color=#ecbe7b><fc=#ecbe7b><action=`alacritty -e htop`>%cpu%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#ff6c6b><fc=#ff6c6b><action=`alacritty -e htop`>%memory%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#a9a1e1><fc=#a9a1e1><action=`alacritty -e htop`>%disku%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#98be65><fc=#98be65>%uparrow%  <action=`alacritty -e htop`>%uptime%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#c678dd><fc=#c678dd>%bell%  <action=`alacritty -e sudo pacman -Syu`>%pacupdate%</action></fc></box>    <box type=Bottom width=2 mb=2 color=#46d9ff><fc=#46d9ff><action=`emacsclient -c -a 'emacs' --eval '(doom/window-maximize-buffer(dt/year-calendar))'`>%date%</action></fc></box> %trayerpad%"
}

