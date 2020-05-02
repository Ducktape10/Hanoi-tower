import Control.Monad.Writer

-- Disks of different sizes
type Disk = Int
-- A rod can have several disks on it
type Rod  = [Disk]
-- A Hanoi problem consists of three rods
type Problem = (Rod, Rod, Rod)
-- Identifier for the rods
data RodID = A | B | C
  deriving (Eq, Ord, Show)
-- Move the topmost disk from one rod to another
type Move = (RodID, RodID)

initial :: Int -> Problem
initial n = ([1..n], [], [])

validateRod :: Rod -> Bool
validateRod (x:y:[]) = x < y
validateRod (x:y:xs)
 | x < y     = validateRod (y:xs)
 | otherwise = False
validateRod _ = True

validateProblem :: Problem -> Bool
validateProblem (a, b, c) = (validateRod a) && validateRod b && validateRod c

safehead :: [a] -> [a]
safehead a = take 1 a

move :: RodID -> RodID -> Problem -> Problem
move A B (a, b, c) = (tail a, (safehead a) ++ b, c)
move A C (a, b, c) = (tail a, b, (safehead a) ++ c)
move B C (a, b, c) = (a, tail b, (safehead b) ++ c)
move B A (a, b, c) = ((safehead b) ++ a, tail b, c)
move C B (a, b, c) = (a, (safehead c) ++ b, tail c)
move C A (a, b, c) = ((safehead c) ++ a, b, tail c)
move _ _ n = n

executeMove :: Move -> Problem -> Problem
executeMove = uncurry move

executeMoves :: [Move] -> Problem -> Problem
executeMoves [] p = p
executeMoves (x:xs) p = executeMoves xs (executeMove x p)

freeRod :: RodID -> RodID -> RodID
freeRod A B = C
freeRod B A = C
freeRod A C = B
freeRod C A = B
freeRod B C = A
freeRod C B = A

type SolverM = Writer [Move]

moveM :: RodID -> RodID -> Problem -> SolverM Problem
moveM a b c = do
  tell $ [(a, b)]
  return (move a b c)

moveManyM :: Int -> RodID -> RodID -> Problem -> SolverM Problem
moveManyM 0 a b c = return c
moveManyM n a b c = do
  l <- moveManyM (n-1) a (freeRod a b) c
  m <- moveM a b l
  moveManyM (n-1) (freeRod a b) b m

solve :: Problem -> [Move]
solve (a, b, c) = execWriter (moveManyM (length a) A C (a, b, c))