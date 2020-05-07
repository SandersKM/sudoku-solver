-- Sudoku Solver
-- Functional Programming Final Project
-- Chloe Baker, Cookie, Kate Sanders
import Data.List (transpose, (\\))

type Matrix a = [[a]]
type Board = Matrix Char
type Choices = [Char]

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols xs = transpose xs

groupByInteger ::  Int -> [a] -> [[a]]
groupByInteger i [] = []
groupByInteger i xs = ys : groupByInteger i yss
    where 
        (ys, yss) = splitAt i xs

group :: [a] -> [[a]]
group xs = groupByInteger (round (sqrt (fromIntegral (length xs)))) xs

ungroup :: [[a]] -> [a]
ungroup = concat

boxes :: Matrix a -> Matrix a
boxes m = map ungroup (ungroup (map cols (group (map group m))))

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

correct :: Board -> Bool
correct b = all nodups (rows b) && all nodups (cols b) && all nodups (boxes b)

choose e = if blank e then cellvals else [e]

choices :: Board -> Matrix Choices
choices = map (map choose)

-- tests whether the argument is a singleton list
single :: [a] -> Bool
single [] = False
single (x:[]) = True
single (x : xs) = False

-- If there is only one option, it sets it to that
fixed :: [Choices] -> Choices
fixed xs = concat (filter single xs)

-- (\\) deletes the first occurance of each thing in a list
remove :: Eq a => [a] -> [a] -> [a]
remove fxd cs = if single cs then cs else cs \\ fxd

-- Takes in a list of choices, css
-- finds the fixed elements in that list of choices,
-- for each Choices in the list of Choices,
-- The fixed elements will be removed
reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css

-- Takes in eaither rows, cols, or boxes
-- then splits it up into the rows cols or boxes
-- maps the reduce function to those groups
-- and then sets them back to their correct position
-- This works because rows, cols, and boxes are all their own inverses!
pruneBy :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
pruneBy f m = f (map reduce (f m))

-- This runs pruneBy on all of the rows, then cols, then boxes
prune :: Matrix Choices -> Matrix Choices
prune b = pruneBy boxes (pruneBy cols (pruneBy rows b))

-- maybe do reduce on the columns and the rows and the boxes

-- prunePal xs = map (remove (pruneBuddy xs)) xs

-- pruneBuddy (x : xs) = if single x then x : pruneBuddy xs else pruneBuddy xs


cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]

mcp :: Matrix [a] -> [Matrix a]
mcp xs = cp (map cp xs)

sudoku :: Board -> [Board]
sudoku b = filter correct (mcp ( prune (choices b)))





boardsize = 4
boxsize = 2
cellvals = "1234"
blank = (== '.')

matrix_a = [['1', '2', '3', '4'], ['3', '4', '1', '2'], ['2', '3', '4', '1'], ['4', '1', '2', '3']]

sudoku_test_1 = [['.', '2', '3', '4'], ['3', '4', '1', '2'], ['2', '3', '4', '1'], ['4', '1', '2', '3']]

sudoku_test_2 = [['.', '2', '3', '4'], ['3', '.', '1', '2'], ['2', '3', '4', '1'], ['4', '1', '2', '3']]

sudoku_test_3 = [['.', '2', '3', '.'], ['.', '.', '1', '2'], ['.', '3', '.', '1'], ['4', '1', '.', '.']]
