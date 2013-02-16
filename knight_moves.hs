import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) = do
    (newX, newY) <- [(x - 2, y + 1),
                     (x - 1, y + 2),
                     (x + 1, y + 2),
                     (x + 2, y + 1),
                     (x - 2, y - 1),
                     (x - 1, y - 2),
                     (x + 1, y - 2),
                     (x + 2, y - 1)]
    guard (newX `elem` [1..8] && newY `elem` [1..8])
    return (newX, newY)

threeMoves :: KnightPos -> [KnightPos]
threeMoves pos = moveKnight pos >>= moveKnight >>= moveKnight

reachableInThree :: KnightPos -> KnightPos -> Bool
reachableInThree start target = target `elem` threeMoves start
