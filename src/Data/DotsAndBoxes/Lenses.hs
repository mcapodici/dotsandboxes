-- Todo remove if we dont need lenses

{-# LANGUAGE TemplateHaskell #-}

module Data.DotsAndBoxes.Lenses where

import Control.Lens.TH
import Data.DotsAndBoxes.Types

makeLenses ''BoardCurrentState
