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
    { cursorFgColour    = Set $ createColour 111 111 111
    , cursorBgColour    = Set $ createColour 111 111 111
    , foregroundColour  = Set $ createColour 111 111 111
    , backgroundColour  = Set $ createColour 111 111 111
    , highlightFgColour = Set $ createColour 111 111 111
    , highlightBgColour = Set $ createColour 111 111 111
    , palette = ExtendedPalette (fromMaybe defaultStandardColours $ mkList8
        [ createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111 ]) $ fromMaybe defaultStandardColours $ mkList8
        [ createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111
        , createColour 111 111 111 ]})

  let newTMConfig = addColourExtension myTMConfig myColourExt

  start newTMConfig
