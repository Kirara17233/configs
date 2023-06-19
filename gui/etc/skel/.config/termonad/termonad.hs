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
    { cursorFgColour    = Set $ createColour  21  21  21
    , cursorBgColour    = Set $ createColour 208 208 208
    , foregroundColour  = Set $ createColour 197 200 198
    , backgroundColour  = Set $ createColour  22  23  25
    , highlightFgColour = Set $ createColour 197 200 198
    , highlightBgColour = Set $ createColour  68  68  68
    , palette = ExtendedPalette (fromMaybe defaultStandardColours $ mkList8
        [ createColour   0   0   0
        , createColour 253  95 241
        , createColour 135 195 138
        , createColour 255 215 177
        , createColour 133 190 253
        , createColour 185 182 252
        , createColour 133 190 253
        , createColour 224 224 224 ]) $ fromMaybe defaultStandardColours $ mkList8
        [ createColour   0   0   0
        , createColour 253  95 241
        , createColour 148 250  54
        , createColour 245 255 168
        , createColour 150 203 254
        , createColour 185 182 252
        , createColour 133 190 253
        , createColour 224 224 224 ]})

  let newTMConfig = addColourExtension myTMConfig myColourExt

  start newTMConfig
