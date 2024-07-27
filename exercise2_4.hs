type Name = String
type Staerke = Float
data Ordnung = Jedi | Imperial
data Charakter = Spieler Name Ordnung Staerke

h1 :: Charakter
h1 = Spieler "Luke" Jedi 7.5
h2 = Spieler "Joda" Jedi 10.0
h3 = Spieler "Vader" Imperial 8.1
h4 = Spieler "Sturmtruppler" Imperial 2.0

--4a)
char_list :: [Charakter]
char_list = [h1, h2, h3, h4]

--4b)
staerke :: Charakter -> Staerke
staerke char = case char of Spieler n o s -> s

--4c)
isJedi :: Charakter -> Bool
isJedi (Spieler n o s) = case o of Jedi -> True;
                                    Imperial -> False

--4d)
getStaerke :: [Charakter] -> Float
getStaerke []  = 0
getStaerke (x:xs) = if isJedi x then (staerke x) + (getStaerke xs) else 0 + (getStaerke xs)

length3 :: [Charakter] -> Float
length3 [] = 0
length3 (x:xs) = if isJedi x then 1.0 + length3(xs) else 0 + length3(xs)

mittlereJediStaerke :: [Charakter] -> Float
mittlereJediStaerke (x:xs) = (getStaerke (x:xs)) /(length3 (x:xs))

-- Berechnung mittlere Stärke von der Tabelle:
-- mittlereJediStaerke char_list 8.75

