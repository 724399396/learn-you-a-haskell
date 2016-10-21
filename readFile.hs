import System.IO

main = do
  handle <- openFile "girlfriend.txt" ReadMode
  contetns <- hGetContents handle
  putStr contetns
  hClose handle