{-# LANGUAGE OverloadedStrings #-}

module Crypto.Passphrase (Dict, dict, en, de, getPass) where

import Prelude hiding (readFile, lines, unwords)
import Control.Applicative
import Control.Monad
import Data.Binary.Strict.Get
import Data.ByteString.Char8 (ByteString, readFile, lines, unwords)
import Data.Monoid
import System.Entropy
import Paths_passphrase (getDataFileName)


newtype Dict = Dict { getWords :: IO [ByteString] }

instance Monoid Dict where
    mempty         = Dict (return mempty)
    d `mappend` d' = Dict $ mappend <$> getWords d <*> getWords d'

dict :: FilePath -> Dict
dict f = Dict $ lines <$> readFile f

dict' :: FilePath -> Dict
dict' lang = Dict $ do
    f <- getDataFileName ("words/" ++ lang ++ ".words")
    getWords (dict f)

en, de :: Dict
en = dict' "en"
de = dict' "de"

getPass :: Dict -> IO ByteString
getPass d = do
    ws  <- getWords d
    ixs <- getRandIxs 4
    return . unwords $ map (\ix -> ws !! (ix `rem` length ws)) ixs
  where
    getRandIxs n = do
        (Right ixs, _) <- runGet (replicateM n getRandIx) <$> getEntropy (n*2)
        return ixs

    getRandIx = do
        x <- fromIntegral <$> getWord8
        y <- fromIntegral <$> getWord8
        return $ 256 * x + y
