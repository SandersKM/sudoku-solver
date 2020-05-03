-- Sudoku Solver
-- Functional Programming Final Project
-- Chloe Baker, Cookie, Kate Sanders
import Data.List (transpose, (\\), length)

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

single :: [a] -> Bool
single [] = False
single (x:[]) = True
single (x : xs) = False

fixed :: [Choices] -> Choices
fixed xs = concat (filter single xs)

remove :: Eq a => [a] -> [a] -> [a]
remove fxd cs = if single cs then cs else cs \\ fxd

reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css

pruneBy :: (Matrix Choices -> Matrix Choices) -> Matrix Choices -> Matrix Choices
pruneBy f m = f (map reduce (f m))

prune :: Matrix Choices -> Matrix Choices
prune b = pruneBy boxes (pruneBy cols (pruneBy rows b))

-- This is checking if there are no single duplicates
-- If there is a single duplicate, safe returns false
-- and thus this board has no solutions
safe :: Matrix Choices -> Bool
safe cm = all  (nodups . fixed) (rows cm) && all (nodups . fixed) (cols cm) && all (nodups . fixed) (boxes cm)

-- returns true if any spot in the matrix is null
void :: Matrix Choices -> Bool
void = any (any null)

-- Checks to see if the matricies given has duplicates or any space that cannot contain a value
-- Blocked matrices never lead to a solution, so we can discard them
blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)

-- Finds the cell with the least number of Choices
minchoice :: Matrix Choices -> Int
minchoice = minimum . filter (1 <) . concat . map (map length) 

expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs] -- puts the list back together
             where (rows1,row : rows2) = break (any best) cm  -- Breaks the matrix of a list of everything before the "best" row and a list of everything after the "best" row and the variable row which is the best row
                   (row1, cs : row2) = break best row  -- Takes in the row containing the "best" and splits it into a list before the "best" and a list of everything else with the "best" as its first element
                   best cs = (length cs == n) -- Method to check if cs is the best choice
                   n = minchoice cm 

search :: Matrix Choices -> [Matrix Choices]
search cm
  |  blocked cm = []  -- Blocked matrices can never lead to a solution
  |  all (all single) cm = [cm] -- If everything in the matrix has one possible choice, it is the only solution
  |  otherwise = (concat . map(search . prune) . expand) cm -- Uses the matrixes that get returned from the expand function, prunes the ones that don't work, and then recursively calls itself to continue the process


cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- cp xss]

mcp :: Matrix [a] -> [Matrix a]
mcp xs = cp (map cp xs)

sudoku :: Board -> [Board]
sudoku = map (map head) . search . prune . choices  -- Uses search to prune choices until it finds the only possibilities for a matrix of choices, and then converts that into a board






boardsize = 4
boxsize = 2
cellvals = "1234"
blank = (== '.')

matrix_a = [['1', '2', '3', '4'], ['3', '4', '1', '2'], ['2', '3', '4', '1'], ['4', '1', '2', '3']]

sudoku_test_1 = [['.', '2', '3', '4'], ['3', '4', '1', '2'], ['2', '3', '4', '1'], ['4', '1', '2', '3']]

sudoku_test_2 = [['.', '2', '3', '4'], ['3', '.', '1', '2'], ['2', '3', '4', '1'], ['4', '1', '2', '3']]

sudoku_test_3 = [['.', '2', '3', '.'], ['.', '.', '1', '2'], ['.', '3', '.', '1'], ['4', '1', '.', '.']]
