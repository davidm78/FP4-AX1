module F95VarDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- Run a parser p on a string str and print the result
run_parser_print :: Show a => Parser a -> String -> IO ()
run_parser_print p str = do
      case parse p "" str of
           Left err -> do
               putStr "parse error at "
               print err
           Right x  -> putStrLn $ "    "++(show x)++","
                                                                                                                                                         
-- Run a parser p on a string str and return the result
run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

f95_var_decl_parser :: Parser VarDecl
f95_var_decl_parser = do whiteSpace
                         typ <- option dummyVarType $ try (type_parser)
                         optional comma
                         dim <- option [] $ try (dim_parser)
                         optional comma
                         intent <- option dummyIntent $ try (intent_parser)
                         optional $ string "::"
                         whiteSpace
                         vars <- option [] $ try (arglist_parser)
                         whiteSpace
                         oclmode <- option dummyArgMode $ try (ocl_argmode_parser)
                         return $ MkVarDecl typ dim intent vars oclmode True
      
type_parser :: Parser VarType
type_parser = do whiteSpace
                 wd <- word
                 num <- option 4 $ try (size_parser)
                 return (case wd of {"integer" -> MkVarType F95Integer num; "real" -> MkVarType F95Real num})

size_parser :: Parser Integer
size_parser = do whiteSpace
                 char '('
                 string "kind"
                 char '='
                 val <- integer
                 char ')'
                 return val
      
dim_parser :: Parser [Range]
dim_parser = do whiteSpace
                string "dimension"
                dims <- parens (commaSep range_parser) <|> return []
                return dims

range_parser :: Parser Range
range_parser = try (range_expr) <|> try (single_expr_range) <|> try (single_const_range) <|> try (single_var_range)

single_var_range :: Parser Range    
single_var_range = do whiteSpace
                      vr <- var_expr
                      return $ MkRange vr vr

single_const_range :: Parser Range
single_const_range = do whiteSpace
                        cr <- const_expr
                        return $ MkRange cr cr

single_expr_range :: Parser Range
single_expr_range = do whiteSpace
                       er <- expr_parser
                       return $ MkRange er er

range_expr :: Parser Range    
range_expr =  do whiteSpace
                 ep <- expr_parser
                 colon
                 ep2 <- expr_parser
                 return $ MkRange ep ep2

intent_parser :: Parser Intent    
intent_parser = do whiteSpace
                   string "intent"
                   wd <- parens(word)
                   return (case wd of {"in" -> In; "out" -> Out; "inout" -> InOut})

   
arglist_parser :: Parser [VarName]    
arglist_parser = try(commaSep1 word) 
                 <|> return []

ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser = do whiteSpace
                        string "!$acc argmode"
                        whiteSpace
                        wd <- word
                        return (case wd of {"read" -> Read; "write" -> Write; "readwrite" -> ReadWrite})

-- Parser for a term in expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
term :: Parser Expr
term = parens expr_parser <|> const_expr <|> var_expr <?> "simple expression"
      
-- Parser for an expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

-- parser for a constant, e.g. 42
const_expr :: Parser Expr
const_expr = do whiteSpace
                con <- integer
                return $ Const con

-- parser for a variable e.g. v
var_expr :: Parser Expr
var_expr = do whiteSpace
              nam <- identifier
              return $ Var nam

-- I suggest you don't touch the code below. It is not dummy code.
optable =
    let
        binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
        prefix name     = Prefix  ( reservedOp  name >> return (\x ->(Pref (MkPrefixOpExpr name x))) ) 
    in
        [
          [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        , [ prefix "-" ]
        ]

lexer       = P.makeTokenParser emptyDef    

parens          = P.parens lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
word            = P.identifier lexer
colon           = P.colon lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
comma           = P.comma lexer
semi            = P.semi lexer
natural         = P.natural lexer

