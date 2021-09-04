{-# LANGUAGE OverloadedStrings #-}
-- | This is an example Termonad configuration that shows how to use the
-- Dracula colour scheme https://draculatheme.com/

module Main where

import Data.ByteString.Internal
import Data.Maybe (fromMaybe)
import GHC.Word
import System.IO
import Termonad
  ( CursorBlinkMode(CursorBlinkModeOn)
  , Option(Set)
  , ShowScrollbar(ShowScrollbarNever)
  , TMConfig
  , confirmExit
  , cursorBlinkMode
  , defaultConfigOptions
  , defaultTMConfig
  , options
  , showMenu
  , showScrollbar
  , start
  , FontConfig
  , FontSize(FontSizePoints)
  , defaultFontConfig
  , fontConfig
  , fontFamily
  , fontSize
  )
import Termonad.Config.Colour
  ( AlphaColour
  , ColourConfig
  , Palette(ExtendedPalette)
  , addColourExtension
  , createColour
  , createColourExtension
  , defaultColourConfig
  , defaultStandardColours
  , defaultLightColours
  , cursorBgColour
  , cursorFgColour
  , backgroundColour
  , foregroundColour
  , highlightBgColour
  , highlightFgColour
  , palette
  , List8
  , mkList8
  )

el :: Int -> String -> GHC.Word.Word8
el index colors = c2w $ last $ take index colors

color :: Int -> String -> AlphaColour Double
color index colors = createColour (el (index * 3 - 2) colors) (el (index * 3 - 1) colors) (el (index * 3) colors)

-- This is our main 'TMConfig'.  It holds all of the non-colour settings
-- for Termonad.
--
-- This shows how a few settings can be changed.
myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar = ShowScrollbarNever
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOn
          , fontConfig =
              defaultFontConfig
                { fontFamily = "MesloLGS NF"
                , fontSize = FontSizePoints 12
                }
          }
    }


main :: IO ()
main = do
  hIn <- openBinaryFile "/etc/colors/main" ReadMode
  colors <- hGetLine hIn
  -- First, create the colour extension based on either PaperColor modules.
  myColourExt <- createColourExtension (defaultColourConfig
    -- Set the default background & foreground colour of text of the terminal.
    { cursorFgColour = Set $ color 1 colors
    , cursorBgColour = Set $ color 2 colors
    , foregroundColour = Set $ color 3 colors  -- black.0
    , backgroundColour = Set $ color 4 colors  -- white.7
    , highlightFgColour = Set $ color 5 colors
    , highlightBgColour = Set $ color 6 colors
    -- Set the extended palette that has 2 Vecs of 8 Dracula palette colours
    , palette = ExtendedPalette (fromMaybe defaultStandardColours $ mkList8
        [ color 7 colors -- black.0
        , color 8 colors -- red.1
        , color 9 colors -- green.2
        , color 10 colors -- yellow.3
        , color 11 colors -- blue.4
        , color 12 colors -- magenta.5
        , color 13 colors -- cyan.6
        , color 14 colors -- white.7
        ]) $ fromMaybe defaultStandardColours $ mkList8
        [ color 15 colors -- black.8
        , color 16 colors -- red.9
        , color 17 colors -- green.10
        , color 18 colors -- yellow.11
        , color 19 colors -- blue.12
        , color 20 colors -- magenta.13
        , color 21 colors -- cyan.14
        , color 22 colors -- white.15
        ]
    })

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
  
