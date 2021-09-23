import Xmobar

cursorFgColor = "#ffffff"
cursorBgColor = "#ffffff"
foregroundColor = "#eeffff"
backgroundColor = "#212121"
highlightFgColor = "#545454"
highlightBgColor = "#eeffff"
black = "#000000"
red = "#ff5370"
green = "#c3e88d"
yellow = "#ffcb6b"
blue = "#82aaff"
purple = "#c792ea"
cyan = "#89ddff"
white = "#ffffff"
lightBlack = "#545454"
lightRed = "#ff5370"
lightGreen = "#c3e88d"
lightYellow = "#ffcb6b"
lightBlue = "#82aaff"
lightPurple = "#c792ea"
lightCyan = "#89ddff"
lightWhite = "#ffffff"

cpu = blue
memory = cyan
storage = green
network = yellow
dateColor = red

main :: IO ()
main = xmobar defaultConfig
  { font = "xft:MesloLGS NF:style=Regular:pixelsize=13"
  , additionalFonts =
      [ "xft:MesloLGS NF:style=Regular:pixelsize=14"
      , "xft:Font Awesome 5 Free,Font Awesome 5 Free Solid:style=Solid:pixelsize=17"
      , "xft:Font Awesome 5 Free,Font Awesome 5 Free Solid:style=Solid:pixelsize=19"
      , "xft:Font Awesome 5 Free,Font Awesome 5 Free Solid:style=Solid:pixelsize=21"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=18"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=20"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=22"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=23" ]
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
      , Run $ Cpu ["-t", "<total>%"] 10
      , Run $ Memory ["-t", "<usedratio>%"] 10
      , Run $ DiskU [("/", "<free>")] [] 10
      , Run $ Com "/etc/config/bin/download.sh" [] "download" 10
      , Run $ Com "/etc/config/bin/upload.sh" [] "upload" 10
      , Run $ Date "%m/%d/%Y" "date" 10 ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<icon=Haskell-White.xpm/><fn=1><fc="++highlightFgColor++">|</fc> %UnsafeStdinReader%</fn>}{"
      ++"<box type=Bottom width=2 mb=1 color="++cpu++"><fc="++cpu++"><fn=7>\xf85a</fn><fn=1> %cpu%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++memory++"><fc="++memory++"><fn=4>\xf538</fn><fn=1> %memory%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++storage++"><fc="++storage++"><fn=6>\xf7c9</fn><fn=1> %disku%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++network++"><fc="++network++"><fn=5>\xf019</fn><fn=1> %download% </fn><fn=5>\xf093</fn><fn=1> %upload%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++dateColor++"><fc="++dateColor++"><fn=2>\xf073</fn><fn=1> %date%</fn></fc></box> "
}
