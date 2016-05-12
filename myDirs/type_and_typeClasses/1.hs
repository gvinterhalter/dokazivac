-- algebarski data type

module Shapes
( Point(..) 
, Shape(..) -- (..) znaci da eksportujemo sve Value konstruktore za Shape
--, Shape (Circle) -- eksportujemo samo Circle konstruktor
--, Shape -- ne eksportujemo ni jedan konstruktor
, surface
, move
, baseCircle
, baseRect
, Osoba(..)
) where

-- Sakrivanjem konstruktor sakriva se implementacija, tipovi su vise abstraktni
-- Sakriveni konstruktori (i ostale f-je) ne mogu da se koriste za patern matching


data Bool = False | True
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float          -- radius
           | Rectangle Point Float Float -- width  & height
           deriving(Show)

-- u ovom primeru: Book, Point i Shape su novi tipovi
-- False, True, Point, Cirlce i Rectangle su "Value konstruktori"
-- Value konstruktor je nista drugo do funkcija koja vraca vrednost data tipa
-- True :: Bool
-- False :: Bool
-- Point :: Float -> Float -> Point
-- Circle :: Point -> Float -> Shape
-- Restangle :: Point -> Point -> Shape

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle _ w h) =  w * h

-- lista krugova razlicitog radijusa
rr = map (Circle (Point 0 0)) [3..5] 

move :: Shape -> Float -> Float -> Shape
move (Circle  (Point x y) r) nx ny = Circle (Point (x+nx) (y+ny)) r
move (Rectangle  (Point x1 y1) w h ) nx ny = Rectangle (Point (x1+nx) (y1+ny)) w h

move2 :: Shape -> Point -> Shape
move2 p (Point x y) = move p x y 

baseCircle r = Circle (Point 0 0) r 
baseRect w h = Rectangle (Point 0 0) w h




-- Record Syntax

-- data Person = Person String Int Float String String deriving (Show)
data Osoba = Osoba { ime :: String
                   , prezime :: String
                   , starost :: Int
                   , visina :: Float
                   , brTelefona :: String
                   } deriving(Show)

-- haskell je automatski napravio funkcije za ekstrakciju imena, prezimena itd..




