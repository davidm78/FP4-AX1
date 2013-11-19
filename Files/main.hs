import Data.Char (toLower)

templ_src_name = "module_LES_ocl_TEMPL.f95"
gen_src_name = "output.f95"

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

main :: IO ()
main = do
	lines <- read_F95_src templ_src_name
	write_F95_src gen_src_name lines
