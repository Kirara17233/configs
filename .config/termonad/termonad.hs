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
    { cursorFgColour    = Set $ createColour  15  17  26
    , cursorBgColour    = Set $ createColour 255 204   0
    , foregroundColour  = Set $ createColour 143 147 162
    , backgroundColour  = Set $ createColour  15  17  26
    , highlightFgColour = Set $ createColour 143 147 162
    , highlightBgColour = Set $ createColour  31  34  51
    , palette = ExtendedPalette (fromMaybe defaultStandardColours $ mkList8
        [ createColour  84 110 122
        , createColour 255  83 112
        , createColour 195 232 141
        , createColour 255 203 107
        , createColour 130 170 255
        , createColour 199 146 234
        , createColour 137 221 255
        , createColour 255 255 255 ]) $ fromMaybe defaultStandardColours $ mkList8
        [ createColour  84 110 122
        , createColour 255  83 112
        , createColour 195 232 141
        , createColour 255 203 107
        , createColour 130 170 255
        , createColour 199 146 234
        , createColour 137 221 255
        , createColour 255 255 255 ]})

  let newTMConfig = addColourExtension myTMConfig myColourExt

  start newTMConfig
