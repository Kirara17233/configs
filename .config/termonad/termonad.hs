{-# LANGUAGE OverloadedStrings #-}

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
  , fontSize )
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
  , mkList8 )

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
                , fontSize = FontSizePoints 12 }}}

main :: IO ()
main = do
  myColourExt <- createColourExtension (defaultColourConfig
    { cursorFgColour    = Set $ createColour 255 255 255
    , cursorBgColour    = Set $ createColour 255 255 255
    , foregroundColour  = Set $ createColour 238 255 255
    , backgroundColour  = Set $ createColour  33  33  33
    , highlightFgColour = Set $ createColour  84  84  84
    , highlightBgColour = Set $ createColour 238 255 255
    , palette = ExtendedPalette (fromMaybe defaultStandardColours $ mkList8
        [ createColour   0   0   0
        , createColour 255  83 112
        , createColour 195 232 141
        , createColour 255 203 107
        , createColour 130 170 255
        , createColour 199 146 234
        , createColour 137 221 255
        , createColour 255 255 255 ]) $ fromMaybe defaultStandardColours $ mkList8
        [ createColour  84  84  84
        , createColour 255  83 112
        , createColour 195 232 141
        , createColour 255 203 107
        , createColour 130 170 255
        , createColour 199 146 234
        , createColour 137 221 255
        , createColour 255 255 255 ]})

  let newTMConfig = addColourExtension myTMConfig myColourExt

  start newTMConfig
