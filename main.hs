
--import System.IO 

--file = ""

--handle = openFile file ReadWriteMode 

--a = getContents file

--b = lines a

--board = map  words b

import System.IO  
  
main = do  
    handle <- openFile "unsolvedBoard.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  