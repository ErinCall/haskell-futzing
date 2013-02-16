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

type ThreeMove = (KnightPos, KnightPos, KnightPos)
moveThree :: KnightPos -> [ThreeMove]
moveThree start = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    return (first, second, third)

reachInThree :: KnightPos -> KnightPos -> Maybe ThreeMove
reachInThree start finish = search finish $ moveThree start
    where search finish [] = Nothing
          search finish (move:moves)
              | finish `finishes` move = Just move
              | otherwise              = search finish moves
          finishes target (one, two, three) = target == three
