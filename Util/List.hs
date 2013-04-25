module Util.List where

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f [] = []
mapFirst f (x:xs) = f x:xs

mapRest :: (a -> a) -> [a] -> [a]
mapRest f [] = []
mapRest f (x:xs) = x:map f xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [] = []
mapLast f [x] = [f x]
mapLast f (x:xs) = x:mapLast f xs

mapLeading :: (a -> a) -> [a] -> [a]
mapLeading f [] = []
mapLeading f [x] = [x]
mapLeading f (x:xs) = f x:mapLeading f xs
