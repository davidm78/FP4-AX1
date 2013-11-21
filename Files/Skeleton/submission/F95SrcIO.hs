{- David Mcinnes 0901288m
Functional Programming 4
Assessed Exercise 1
21/11/13
This file is fully working -}

module F95SrcIO (
		read_F95_src,
        write_F95_src
) where
import Data.Char (toLower)
-- Fortran is case-insensitive so turn everything into lowercase
lc = map toLower

-- given the name of the file, read it into a list of strings, one per line of source code
read_F95_src :: String -> IO [String]
read_F95_src src_name = do
			 sourceFile <- readFile src_name
			 let sourceLines = lines sourceFile
			 return (map lc sourceLines)

-- given a list of strings, one per line of source code, and the name of the file, write the strings to the file
write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines = writeFile src_name (unlines src_lines)