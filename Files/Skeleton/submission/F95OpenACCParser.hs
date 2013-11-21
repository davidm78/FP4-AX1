module F95OpenACCParser (
    extract_OpenACC_regions_from_F95_src
) where
import Text.Regex.Posix -- suggest use of regular expressions

-- given the source code as a list of lines (strings), extract the OpenACC regions for Arguments and ConstArguments as well as the parameter declarations, and return them as a tuple of three lists of strings, in that order.
extract_OpenACC_regions_from_F95_src :: [String] -> ([String],[String],[String])
extract_OpenACC_regions_from_F95_src in_src_lines = (extract_arguments in_src_lines False, extract_constarguments in_src_lines False, extract_parameter_declarations in_src_lines False)

-- given the source code as a list of lines, extract openACC regions for arguments
extract_arguments :: [String] -> Bool -> [String]
extract_arguments [] x = []
extract_arguments(x:xs) False = if x =~ "!\\$acc arguments"
								then extract_arguments xs True
								else extract_arguments xs False
extract_arguments(x:xs) True = if x =~ "!\\$acc end arguments"
							   then extract_arguments xs False
							   else x:extract_arguments xs True

-- given the source code as a list of lines, extract openACC regions for constarguments
extract_constarguments :: [String] -> Bool -> [String]
extract_constarguments [] x = []
extract_constarguments(x:xs) False = if x =~ "!\\$acc constarguments"
									 then extract_constarguments xs True 
									 else extract_constarguments xs False
extract_constarguments(x:xs) True = if x =~ "!\\$acc end constarguments"
									then extract_constarguments xs False
									else x:extract_constarguments xs True

-- given the source code as a list of lines, extract the perameter declarations
extract_parameter_declarations :: [String] -> Bool -> [String]
extract_parameter_declarations [] x = []
extract_parameter_declarations(x:xs) False = if x =~ "(parameter)"
									   then extract_parameter_declarations xs True
									   else extract_parameter_declarations xs False
extract_parameter_declarations(x:xs) True = if x =~ "(parameter)"
											then x:extract_parameter_declarations xs True
											else extract_parameter_declarations xs False
