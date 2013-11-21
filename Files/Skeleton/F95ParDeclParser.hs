module F95ParDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import F95VarDeclParser

-- parse a parameter declaration string into a ParDecl 
f95_par_decl_parser :: Parser ParDecl
f95_par_decl_parser = do whiteSpace
                      	 typ <- option dummyVarType $ try (type_parser)
                         comma
                         whiteSpace
                      	 string "parameter"
                      	 dim <- option [] $ try (dim_parser)
                      	 whiteSpace
                      	 optional $ string "::"
                      	 whiteSpace
                      	 var <- identifier
                      	 string "="
                      	 val <- const_expr
                      	 return $ MkParDecl typ dim var val