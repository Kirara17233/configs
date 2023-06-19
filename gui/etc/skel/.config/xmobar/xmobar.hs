import Xmobar

cursorFgColor = "#151515"
cursorBgColor = "#d0d0d0"
foregroundColor = "#c5c8c6"
backgroundColor = "#161719"
highlightFgColor = "#c5c8c6"
highlightBgColor = "#444444"
black = "#000000"
red = "#fd5ff1"
green = "#87c38a"
yellow = "#ffd7b1"
blue = "#85befd"
purple = "#b9b6fc"
cyan = "#85befd"
white = "#e0e0e0"
lightBlack = "#000000"
lightRed = "#fd5ff1"
lightGreen = "#94fa36"
lightYellow = "#f5ffa8"
lightBlue = "#96cbfe"
lightPurple = "#b9b6fc"
lightCyan = "#85befd"
lightWhite = "#e0e0e0"

cpu = blue
memory = cyan
storage = green
network = yellow
dateColor = red

main :: IO ()
main = xmobar defaultConfig
  { font = "xft:MesloLGS NF:style=Regular:pixelsize=13"
  , additionalFonts =
      [ "xft:Noto Sans CJK SC:style=Regular:pixelsize=15"
      , "xft:Font Awesome 6 Free,Font Awesome 6 Free Solid:style=Solid:pixelsize=17"
      , "xft:Font Awesome 6 Free,Font Awesome 6 Free Solid:style=Solid:pixelsize=19"
      , "xft:Font Awesome 6 Free,Font Awesome 6 Free Solid:style=Solid:pixelsize=21"
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
  , iconRoot = "/configs/icons"
  , allDesktops = True
  , commands = 
      [ Run $ UnsafeStdinReader
      , Run $ Cpu ["-t", "<total>%"] 10
      , Run $ Memory ["-t", "<usedratio>%"] 10
      , Run $ DiskU [("/", "<free>")] [] 10
      , Run $ Com "download.sh" [] "download" 10
      , Run $ Com "upload.sh" [] "upload" 10
      , Run $ Date "%m/%d/%Y" "date" 10 ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<icon=Haskell-White.xpm/><fn=1><fc="++foregroundColor++"> | </fc>%UnsafeStdinReader%</fn>}{"
      ++"<box type=Bottom width=2 mb=1 color="++cpu++"><fc="++cpu++"><fn=7>\xf85a</fn><fn=1> %cpu%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++memory++"><fc="++memory++"><fn=4>\xf538</fn><fn=1> %memory%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++storage++"><fc="++storage++"><fn=6>\xf7c9</fn><fn=1> %disku%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++network++"><fc="++network++"><fn=5>\xf019</fn><fn=1> %download% </fn><fn=5>\xf093</fn><fn=1> %upload%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=1 color="++dateColor++"><fc="++dateColor++"><fn=2>\xf073</fn><fn=1> %date%</fn></fc></box>"
}
