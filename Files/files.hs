import Data.Char (toLower)
-- Fortran is case-insensitive so turn everything into lowercase
lc = map toLower

-- given the name of the file, read it into a list of strings, one per line of source code
read_F95_src :: String -> IO [String]
read_F95_src src_name = do
			 sourceFile <- readFile
			 sourceLines <- lines (map lc sourceFile)
		return sourceLines

-- given a list of strings, one per line of source code, and the name of the file, write the strings to the file
write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines =  do
		fileString <- unlines src_lines
		writeFile src_name (fileString)

module Main where 
	
main :: IO ()
main = do
	lines <- read_F95_src "module_LES_ocl_TEMPL.f95"
	writeFile "output.f95"

