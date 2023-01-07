
type Staerke = Float
data Ordnung = Jedi | Imperial
    deriving (Eq, Show)
data Charakter = Spieler Name Ordnung Staerke
    deriving(Eq, Show)

liste_char :: [Charakter]
liste_char = [Spieler "Luke" Jedi 7, Spieler "Yoda" Jedi 10,
 Spieler "Vader" Imperial 12, Spieler "Ein Sturmtruppler" Imperial 2]


isJedi :: Charakter -> Bool
isJedi (Spieler n Jedi s) = True
isJedi (Spieler _ _ _) = False

staerke :: Charakter -> Staerke
staerke (Spieler n o s) = s

besterJedi :: [Charakter] -> Maybe Charakter
besterJedi (x:xs) = besterJedi2 (filter isJedi (x:xs))
besterJedi [] = besterJedi2 []

besterJedi2 :: [Charakter] -> Maybe Charakter
besterJedi2 (x:y:ys) = if staerke x > staerke y then besterJedi2 (x:ys) else besterJedi2 (y:ys)
besterJedi2 [x] = Just x
besterJedi2 [] = Nothing

filterJedi :: [Charakter] -> [Charakter]
filterJedi (x:xs) = filter isJedi (x:xs)
filterJedi [] = []

map_staerke :: [Charakter] -> [Staerke]
map_staerke (x:xs) = map staerke (x:xs)
map_staerke [] = []

mittlerstaerke :: [Charakter] -> Float
mittlerstaerke xs = if filter isJedi xs == [] then 0.0 else foldr (+) 0.0 (map_staerke (filterJedi xs)) / fromIntegral (length (filterJedi xs))


print_jedi :: Charakter -> Name
print_jedi (Spieler n o s) = n

liste2 = [1,2,3,4,45]
liste3 = [88,12,1]
liste4 = [1]
liste5 = []


max_liste2 :: [Integer] -> Integer
max_liste2 [] = 0
max_liste2 (x:y:ys) = if x > y then max_liste2 (x:ys) else max_liste2 (y:ys)
max_liste2 [x]= x

sum_liste :: [Integer] -> Integer
sum_liste (x:xs) = foldr (+) 0 (x:xs)
sum_liste [] = 0


length_int :: [Integer] -> Integer
length_int (x:xs) = 1 + length_int (xs)
length_int [] = 0


durchschnitt :: [Integer] -> Integer
durchschnitt [] = 0
durchschnitt (x:xs) =  (foldr (+) 0 (x:xs)) `div` length_int (x:xs)

reverse_liste :: [a] -> [a]
reverse_liste = foldl (\auf x -> x : auf) []

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 a (x:xs) = if x == a then True else elem2 a xs
elem2 _ [] = False


doppel :: (Eq a) => [a] -> [a]
doppel (x:xs) = if elem2 x xs then doppel xs else x:doppel xs
doppel [] = []

doppel2 :: (Eq a) => [a] -> [a]
doppel2 [] = []
doppel2 (x:xs)
 | x `elem` xs = doppel2 xs
 | otherwise = x : doppel2 xs


isAsc2 :: [Integer] -> Bool
isAsc2 (x:y:ys) = if x<=y then True && isAsc2 (y:ys) else False
isAsc2 [x] = True
isAsc2 [] = True

sum_liste2 :: [Integer] -> [[Integer]]
sum_liste2 [] = []
sum_liste2 (x:y:z:xs) = [y,z,x, x+y+z] : sum_liste2 xs
sum_liste2 (x:y:ys) = [x,y, x+y] : sum_liste2 ys
sum_liste2 [x] = [[x,x]]

--sum_tupel :: [(Int, Int)] -> [(Int, Int)]
--sum_tupel [(a,b):xs] = (b,a) : sum_tupel xs
--sum_tupel [] = [(0, 0)]


greet :: IO ()
greet = do
    putStrLn "Wie heißt du?"
    name <- getLine
    putStrLn "Wie spät ist es?"
    uhrzeit <- getLine
    putStrLn ("Hallo " ++ name ++ "." ++ " Und es ist " ++ uhrzeit ++ " Uhr.")



count :: Int -> Int -> IO ()
count n m = do
    putStrLn ("Drücke Enter um eins hochzuzählen.")
    i <- getLine
    if i == "quit" then return ()
    else putStrLn (show n)
    if n < m then count (n+1) m
    else return ()

count2 :: Int -> IO ()
count2 n = do
    putStrLn ("Drücke Enter um eins hochzuzählen.")
    m <- getLine
    if m == "quit" then return ()
    else putStrLn (show n)
    if n < 999 then count2 (n+1)
     else return ()


g xs = foldl (\x y->x-y) 8 xs

g_scan xs = scanl (\x y -> x - y) 8 xs

g_2 xs = foldr (\x y->x-y) 8 xs

g_3 xs = scanr (\x y->x-y) 8 xs

g_4 xs = scanr (\x y -> x / y) 8 xs

g_5 xs = scanr (/) 8 xs

g_6 xs = scanr1 (+) xs

g_7 xs = scanl1 (/) xs

multiply_1 [] = 0
multiply_1 [x] = x
multiply_1 (x:xs) = x*multiply_1 xs

summe :: [Int] -> Int
summe [] = 0
summe (x:xs) = x + sum xs

drop_1 :: Int -> [a] -> [a]
drop_1 y (x:xs) = if y == 0 then (x:xs) else drop_1 (y-1) (xs)
drop_1 _ [] = []

last_element_list :: [a] -> [a]
last_element_list [x] = []
last_element_list (x:xs) = x : last_element_list (xs)


rev_1 :: [a] -> [a]
rev_1 (x:xs) = foldl (\y x -> x : y) [] (x:xs)
rev_1 [] = []

xss = [(2,3), (3,2), (4,3), (9,1)]

list78 = [(x,y) | (x,y) <- xss, rem y 3 /= 0, x > y]

data Hochhaus=Hochhaus Name Hoehe Nutzung Etagen Baujahr
    deriving(Eq,Show)
type Name = String
type Hoehe = Int
type Etagen = Int
type Baujahr=Int
data Nutzung = Bueros | Wohnungen | Hotel | Mehrzweck
    deriving(Eq,Show)
data Block=Block [Hochhaus] [Strasse] [Buslinie]
    deriving(Eq,Show)
type Strasse = String
type Buslinie = String


häuserblock = Block [Hochhaus "Chrysler Building" 319 Bueros 77 1930, Hochhaus "Chrysler Building East" 132 Bueros 32 1999] ["Lexingtion Ave", "E 42nd St", "E 43rd St", "3rd Ave"] ["M42", "M101", "M102", "M103", "X21", "X63", "X64", "X68"]

hoehe :: Hochhaus -> Hoehe
hoehe (Hochhaus n h nu e b) = h

max_hoehe :: [Hoehe] -> Hoehe
max_hoehe [] = 0
max_hoehe [x] = x
max_hoehe (x:y:ys) = if x > y then max_hoehe (x:ys) else max_hoehe (y:ys)



maximaleHoehe :: Block -> Hoehe
maximaleHoehe (Block (x:xs) s b) = max_hoehe (map hoehe (x:xs))



namehochhaus :: Hochhaus -> Name
namehochhaus (Hochhaus n h nu e b) = n


ersetze_2 :: String -> Hochhaus -> [Hochhaus] -> [Hochhaus]
ersetze_2 _ h [] = []
ersetze_2 x h (y:ys) = if x == namehochhaus y then (h:ys) else [y] ++ ersetze_2 x h (ys)



ersetzeHochhaus :: String -> Hochhaus -> Block -> Block
ersetzeHochhaus _ _ (Block [] s b) = Block [] s b
ersetzeHochhaus s h (Block (x:xs) o b) = Block (ersetze_2 s h (x:xs)) o b


--data Brief = Brief Abmessung Gewicht
    --deriving (Eq, Show)
--type Abmessung = (Laenge, Breite, Hoehe)
--type Laenge = Int
--type Breite = Int
--type Gewicht = Int

--beispiel = Brief (100, 50, 10) 15

--beispiel_liste = [beispiel, Brief (501, 50, 10) 18, Brief (200, 30, 10) 4000]
--volumen :: Brief -> Int
--volumen (Brief (l, b, h) _) = l * b *h

--länge_b :: Brief -> Laenge
--länge_b (Brief (l, _, _) _) = l
--gewicht_b :: Brief -> Gewicht
--gewicht_b (Brief (_, _, _) g) = g

--ungueltig :: [Brief] -> [Brief]
--ungueltig (x:xs) = if (länge_b x) > 500 || (gewicht_b) x > 3000 then [x] ++ ungueltig (xs) else ungueltig (xs)
--ungueltig [] = []

multiply_4 :: [Integer] -> Integer
multiply_4 [] = 1
multiply_4 (x:xs) = x* multiply_4 (xs)


halbiere2 :: (Double -> Double) -> Double -> Double -> Double
halbiere2 f l r
    |abs (l-r) < 10e-9 = c
    |f(l) * f(c) < 0 = halbiere2 f l c
    | f l * f r > 0 = error "error"
    |otherwise = halbiere2 f c r
    where c = (l+r)/2

ausprobieren :: Int -> Int
ausprobieren x = if (x `rem` 3 == 0) then 5 else 10


tuple_liste :: [(Integer, Integer)] -> Integer
tuple_liste [] = 0
tuple_liste ((x,y):ys) = x+y+ tuple_liste (ys)