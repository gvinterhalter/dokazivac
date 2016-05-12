
-- Type  parameters



data Object a = Object { name :: String,  desc :: a} 
               deriving(Show)


o1 = Object "file 1" 0.2342
o2 = Object "file 2" [1..10]
o3 = Object "file 2" "Descripton nije dostupan"

-- key za mapu mora da bude Ord tipa
data (Ord k) => Map k v = Map String k v


data Vector a = Vector [a] deriving (Show, Eq, Ord)

vecComb f (Vector [a]) (Vector [b])  = 
 Vector [f x y | (x, y) <- zip a b]






-- type classes
-- type class je <=> Interfejs u OO metodologiji

data Osoba = Osoba { ime :: String
                   , prezime :: String
                   , starost :: Int
                   } deriving (Eq, Show, Read)

data Person = Child String
            | Person String
            | OldPerson String
            deriving (Eq, Ord, Show)

-- Child "Mika" < Person "Mika" 
-- Person"Mika" < OldPerson "Mika" 

-- Enum class ima sledbenika i prethodnika
-- Bounded class ima min i max vrednost

data Dan = Ponedeljak | Utorak | Sreda | Cetvrtak
         | Poetak | Subota | Nedelja
         deriving(Eq, Ord, Show, Read, Bounded, Enum)
--
-- succ Sreda 
-- pred Sreda 
-- maxBound :: Day 
-- minBound :: Day 


type BrojTelefona = String
type Ime = String
type Imenik = [(Ime, BrojTelefona)]
