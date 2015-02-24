{-|
Module      : Conversions
Description : Functions for translating between GGg and ASCII
Copyright   : (c) Carter Hinsley, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module Conversions
( asciiTextToG
, gTextToAscii
) where

import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)

-- | Convert an ASCII character to a GGg word. If not a known GGg word, convert
-- the Char to a String.
asciiToG :: Char -> String
asciiToG a = fromMaybe [a] $ Bimap.lookup a lookupTable

-- | Convert a GGg word to an ASCII character. If not a known GGg word, return
-- the first Char of the GGg String.
gToAscii :: String -> Char
gToAscii g = fromMaybe (head g) $ Bimap.lookupR g lookupTable

-- | Convert an ASCII string to a GGg string using `asciiToG`.
asciiTextToG :: String -> String
asciiTextToG  = drop 1
              . foldl (++) ""
              . map (\c -> (if c == ' ' then "" else " ") ++ asciiToG c)

-- | Convert a GGg string to an ASCII string using `gToAscii`.
gTextToAscii :: String -> String
gTextToAscii = foldr ((:) . gToAscii) ""
             . words
             . concatMap id
             . map (\c -> if c == ',' then " ," else [c])

-- | Data.Bimap.Bimap containing translation mappings for ASCII->GGg and vice
-- versa.
lookupTable :: Bimap.Bimap Char String
lookupTable =
    Bimap.fromList [ ('a', "gG")
                   , ('b', "Gggg")
                   , ('c', "GgGg")
                   , ('d', "Ggg")
                   , ('e', "g")
                   , ('f', "ggGg")
                   , ('g', "GGg")
                   , ('h', "gggg")
                   , ('i', "gg")
                   , ('j', "gGGG")
                   , ('k', "GgG")
                   , ('l', "gGgg")
                   , ('m', "GG")
                   , ('n', "Gg")
                   , ('o', "GGG")
                   , ('p', "gGGg")
                   , ('q', "GGgG")
                   , ('r', "gGg")
                   , ('s', "ggg")
                   , ('t', "G")
                   , ('u', "ggG")
                   , ('v', "gggG")
                   , ('w', "gGG")
                   , ('x', "GggG")
                   , ('y', "GgGG")
                   , ('z', "GGgg")
                   , ('A', "g-gG")
                   , ('B', "g-Gggg")
                   , ('C', "g-GgGg")
                   , ('D', "g-Ggg")
                   , ('E', "g-g")
                   , ('F', "g-ggGg")
                   , ('G', "g-GGg")
                   , ('H', "g-gggg")
                   , ('I', "g-gg")
                   , ('J', "g-gGGG")
                   , ('K', "g-GgG")
                   , ('L', "g-gGgg")
                   , ('M', "g-GG")
                   , ('N', "g-Gg")
                   , ('O', "g-GGG")
                   , ('P', "g-gGGg")
                   , ('Q', "g-GGgG")
                   , ('R', "g-gGg")
                   , ('S', "g-ggg")
                   , ('T', "g-G")
                   , ('U', "g-ggG")
                   , ('V', "g-gggG")
                   , ('W', "g-gGG")
                   , ('X', "g-GggG")
                   , ('Y', "g-GgGG")
                   , ('Z', "g-GGgg")
                   , ('0', "GGGGG")
                   , ('1', "gGGGG")
                   , ('2', "ggGGG")
                   , ('3', "gggGG")
                   , ('4', "ggggG")
                   , ('5', "ggggg")
                   , ('6', "Ggggg")
                   , ('7', "GGggg")
                   , ('8', "GGGgg")
                   , ('9', "GGGGg")
                   , ('.', "g'gG")
                   , (',', "g'Gg")
                   , (':', "g'GG")
                   , (';', "g'gg")
                   , ('!', "ggGGg")
                   , ('?', "ggGGgg")
                   , ('\'', "gGGGGg")
                   , ('-', "GggggG")
                   , ('/', "GggGg")
                   , ('"', "gGggGg")
                   , ('@', "gGGgGg")
                   , ('#', "GggGgGgG")
                   , ('$', "GGGGgGgG")
                   , ('%', "gGGggggg")
                   , ('^', "ggGGgggG")
                   , ('&', "GggGgggg")
                   , ('*', "GGgGgggg")
                   , ('|', "ggggggggg")
                   , ('(', "ggggggggG")
                   , (')', "gggggggGg")
                   , ('[', "gggggggGG")
                   , (']', "ggggggGgg")
                   , ('{', "ggggggGgG")
                   , ('}', "ggggggGGg")
                   , ('<', "ggggggGGG")
                   , ('>', "gggggGggg")
                   , ('\\', "gggggGggG")
                   , ('`', "gggggGgGg")
                   , ('~', "gggggGgGG")
                   , ('=', "GgggG")
                   , ('_', "ggGGgG")
                   , (' ', ",")
                   ]

