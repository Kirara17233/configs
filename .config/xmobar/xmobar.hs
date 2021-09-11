import Xmobar

main :: IO ()
main = xmobar defaultConfig {
  font = "xft:MesloLGS NF:style=Regular:pixelsize=10"
  , additionalFonts = []
  , bgColor = "black"
  , fgColor = "grey"
  , position = TopSize L 50 21
  , lowerOnStart = True
  , pickBroadest = False
  , persistent = True
  , hideOnStart = False
  , iconRoot = "/etc/config/icons/"
  , allDesktops = True
  , commands = [ Run $ Weather "EGPH" ["-t","<station>: <tempC>C",
                                        "-L","18","-H","25",
                                        "--normal","green",
                                        "--high","red",
                                        "--low","lightblue"] 36000
               , Run $ Network "eth0" ["-L","0","-H","32",
                                        "--normal","green","--high","red"] 10
               , Run $ Network "eth1" ["-L","0","-H","32",
                                        "--normal","green","--high","red"] 10
               , Run $ Cpu ["-L","3","-H","50",
                             "--normal","green","--high","red"] 10
               , Run $ Memory ["-t","Mem: <usedratio>%"] 10
               , Run $ Swap [] 10
               , Run $ Com "uname" ["-s","-r"] "" 36000
               , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
              ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "<icon=Arch.xpm/>"
}
