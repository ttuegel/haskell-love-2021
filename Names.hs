{-# LANGUAGE ImportQualifiedPost #-}

module Names where

import Control.Monad (replicateM)
import Data.Char (toUpper)
import System.Random

mkName :: IO String
mkName = replicateM 4 mkChar

mkChar :: IO Char
mkChar = do
    upper <- randomIO
    char <- randomRIO ('a', 'z')
    pure (if upper then toUpper char else char)
