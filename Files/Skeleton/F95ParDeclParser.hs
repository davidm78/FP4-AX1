module F95ParDeclParser 
where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import F95VarDeclParser

-- parse a parameter declaration string into a ParDecl 
f95_par_decl_parser :: Parser ParDecl
f95_par_decl_parser = do whiteSpace
                      	 typ <- word
                         comma
                      	 string "parameter"
                      	 whiteSpace
                      	 string "::"
                      	 nam <- var_expr
                      	 string "="
                      	 val <- const_expr
                      	 return (case typ of {"integer" -> MkParDecl F95Integer [] nam val; "real" -> MkParDecl F95Real [] nam val})