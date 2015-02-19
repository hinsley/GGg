module Conversions where

import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)

asciiToG :: Char -> String
asciiToG a = fromMaybe [a] $ Bimap.lookup a lookupTable

gToAscii :: String -> Char
gToAscii g = fromMaybe (g !! 0) $ Bimap.lookupR g lookupTable

asciiTextToG :: String -> String
asciiTextToG  = drop 1
              . foldl (++) ""
              . map (\c -> (if c == ' ' then "" else " ") ++ asciiToG c)

gTextToAscii :: String -> String
gTextToAscii = foldr (:) ""
             . map gToAscii
             . words
             . concatMap id
             . map (\c -> if c == ',' then " ," else [c])

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

