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

cpu = blue
memory = green
storage = yellow
dateColor = red

main :: IO ()
main = xmobar defaultConfig
  { font = "xft:MesloLGS NF:style=Regular:pixelsize=13"
  , additionalFonts =
      [ "xft:MesloLGS NF:style=Regular:pixelsize=14"
      , "xft:Font Awesome 5 Free,Font Awesome 5 Free Regular:style=Regular:pixelsize=17"
      , "xft:Font Awesome 5 Free,Font Awesome 5 Free Solid:style=Solid:pixelsize=17"
      , "xft:Font Awesome 5 Brands,Font Awesome 5 Brands Regular:style=Regular:pixelsize=17"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=18"
      , "xft:Symbols Nerd Font:style=2048-em:pixelsize=21" ]
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
      , Run $ Memory ["-t", "<used>/<total>M <usedratio>%"] 10
      , Run $ DiskU [("/", "<used>/<size>")] [] 10
      , Run $ Date "%m/%d/%Y" "date" 10 ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<icon=Haskell-White.xpm/><fn=1><fc="++highlightFgColor++">|</fc> %UnsafeStdinReader%</fn>}{"
      ++"<box type=Bottom width=2 mb=2 color="++cpu++"><fc="++cpu++"><fn=6>\xf85a</fn><fn=1> %cpu%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=2 color="++memory++"><fc="++memory++"><fn=3>\xf538</fn><fn=1> %memory%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=2 color="++storage++"><fc="++storage++"><fn=5>\xf7c9</fn><fn=1> %disku%</fn></fc></box> "
      ++"<box type=Bottom width=2 mb=2 color="++dateColor++"><fc="++dateColor++"><fn=3>\xf073</fn><fn=1> %date%</fn></fc></box> "
}
