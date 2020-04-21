-- Sudoku Solver
-- Functional Programming Final Project
-- Chloe Baker, Cookie, Kate Sanders

-- sudoku :: Board → [Board]

-- A Matrix is a list of lists containing elements of type a
-- The Board will be an N^2 x N^2 matrix
type Matrix a = [[a]]

-- Since our input for boards is in a .txt file, we make it into a Matrix of characters
type Board = Matrix Char

-- TODO: Function that rejects invalid boards (some elements aren't in cellvals)

-- For now, we assume the board satisfied basic requirements

