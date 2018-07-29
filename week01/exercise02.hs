type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disk source dest aux
    | disk == 1 = [(source, dest)]
    | otherwise = (hanoi((-1 +) disk) source aux dest) ++ [(source, dest)] ++ (hanoi((-1 +) disk) aux dest source)
