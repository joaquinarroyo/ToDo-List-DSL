{
module Parser.Parser where
import Filter.AST
import Command.AST 
import Structures.Task
import Structures.Folder
import Structures.Route
import Structures.Env
import Extra.Lib as L
import Data.Maybe
import Data.Char
import Prelude 
}

%monad { P } { thenP } { returnP }
%name term Exp
%name comm Comm

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='         { TEquals }
    ':'         { TColon }
    '('         { TOpen }
    ')'         { TClose }
    '>'         { TGt }
    '<'         { TLt }
    ">="        { TGte }
    "<="        { TLte }
    '/'         { TSlash }
    ','         { TComma }
    '.'         { TBack }
    NAME        { TName }
    DESCRIPTION { TDescription }
    COMPLETED   { TCompleted }
    PRIORITY    { TPriority }
    TIMESTAMP   { TTimestamp }
    NEQ         { TNequals }
    AND         { TAnd }
    OR          { TOr }
    NOT         { TNot }
    VAR         { TVar $$ }
    NUM         { TNum $$ }
    TRUE        { TTrue }
    FALSE       { TFalse }
    NEWTASK     { TNewTask }
    DELETETASK  { TDeleteTask }
    EDITTASK    { TEditTask }
    COMPLETETASK { TCompleteTask }
    NEWDIR      { TNewDir }
    EDITDIR     { TEditDir }
    DELETEDIR   { TDeleteDir }
    LS          { TLS }
    CD          { TCD }
    SEARCH      { TSearch }
    REC         { TRec }
    ILIKE       { TILike }
    EXIT        { TExit }

%right VAR
%left '=' '>' '<' NEQ
%left AND OR
%right NOT

%%

Comm    : NEWTASK '(' Oration ',' Oration ',' Num ',' Date ')'  { NewTask $3 $5 $7 $9 }
        | NEWTASK '(' Oration ',' Oration ',' Num ')'           { NewTask $3 $5 $7 ""}
        | NEWTASK '(' Oration ',' Oration ',' Date ')'          { NewTask $3 $5 0 $7 }
        | NEWTASK '(' Oration ',' Oration ')'                   { NewTask $3 $5 0 ""}
        | DELETETASK Oration                                    { DeleteTask $2 }
        | EDITTASK Oration Field Oration                        { EditTask $2 $3 $4 }
        | EDITTASK Oration COMPLETED Bool                       { EditTaskB $2 $4 }
        | EDITTASK Oration PRIORITY Num                         { EditTaskP $2 $4 }
        | EDITTASK Oration TIMESTAMP Date                       { EditTaskT $2 $4 }
        | COMPLETETASK Oration                                  { EditTaskB $2 True }
        | NEWDIR Oration                                        { NewDir $2 }
        | EDITDIR Oration                                       { EditDir $2 }
        | DELETEDIR Oration                                     { DeleteDir $2 }
        | LS                                                    { LS }
        | CD Route                                              { CD $2 }
        | SEARCH REC Exp                                        { Search $3 True} 
        | SEARCH Exp                                            { Search $2 False}
        | EXIT                                                  { Exit }

Route   : VAR '/' Route { Route $1 $3 }
        | VAR           { Route $1 Empty } 
        | '.'           { Back }

Exp     : Field '=' Oration          { FieldEq $1 $3 }
        | COMPLETED '=' Bool         { FieldEqB $3 }
        | PRIORITY '=' Num           { FieldEqP $3 }
        | TIMESTAMP '=' Date         { FieldEqT $3 }
        | Field ILIKE Oration        { FieldIlike $1 $3 }        
        | Field NEQ Oration          { FieldNEq $1 $3 }
        | COMPLETED NEQ Bool         { FieldNEqB $3 }
        | PRIORITY NEQ Num           { FieldNEqP $3 }
        | TIMESTAMP NEQ Date         { FieldNEqT $3 }
        | PRIORITY '>' Num           { FieldGtP $3 }
        | PRIORITY '<' Num           { FieldLtP $3 }
        | PRIORITY ">=" Num          { FieldGteP $3 }
        | PRIORITY "<=" Num          { FieldLteP $3 }
        | TIMESTAMP '>' Date         { FieldGtT $3 }
        | TIMESTAMP '<' Date         { FieldLtT $3 }
        | TIMESTAMP ">=" Date        { FieldGteT $3 }
        | TIMESTAMP "<=" Date        { FieldLteT $3 }
        | Exp AND Exp                { And $1 $3 }
        | Exp OR Exp                 { Or $1 $3 }
        | NOT Exp                    { Not $2 }
        | '(' Exp ')'                { $2 }
        
Field   : NAME                       { Name }
        | DESCRIPTION                { Description }

Oration : VAR Oration { $1 ++ " " ++ $2 }
        | VAR         { $1 }

Date    : NUM '/' NUM '/' NUM  NUM ':' NUM  { ($1 ++ "-" ++ $3 ++ "-" ++ $3 ++ " " ++ $3 ++ ":" ++ $8) }
        | NUM '/' NUM '/' NUM               { ($1 ++ "-" ++ $3 ++ "-" ++ $3) }

Num     : NUM     { read $1 }

Bool    : TRUE                  { True }
        | FALSE                 { False }
        
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Comando invalido "++ (s)

data Token =      TVar String
                | TNum String
                | TName
                | TDescription
                | TCompleted
                | TPriority
                | TTimestamp
                | TAnd
                | TOr
                | TNot
                | TEquals
                | TOpen
                | TClose
                | TSlash
                | TColon
                | TComma
                | TBack
                | TNequals
                | TTrue
                | TFalse
                | TGt
                | TLt
                | TGte
                | TLte
                | TNewTask
                | TDeleteTask
                | TEditTask
                | TCompleteTask
                | TNewDir
                | TEditDir
                | TDeleteDir
                | TLS
                | TCD
                | TSearch
                | TRec
                | TILike
                | TExit
                | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | c == '-' -> lexFlag (c:cs)
                          | isAlpha c -> lexVar (c:cs)
                          | isDigit c -> lexNum (c:cs)
                          | otherwise -> lexSym (c:cs)
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlphaNum cs of
                              (s, rest) -> case L.toLower s of
                                        "name" -> cont TName rest
                                        "description" -> cont TDescription rest
                                        "completed" -> cont TCompleted rest
                                        "priority" -> cont TPriority rest
                                        "timestamp" -> cont TTimestamp rest
                                        "and" -> cont TAnd rest
                                        "or" -> cont TOr rest
                                        "not" -> cont TNot rest
                                        "true" -> cont TTrue rest
                                        "false" -> cont TFalse rest
                                        "newtask" -> cont TNewTask rest
                                        "deletetask" -> cont TDeleteTask rest
                                        "edittask" -> cont TEditTask rest
                                        "completetask" -> cont TCompleteTask rest
                                        "newdir" -> cont TNewDir rest
                                        "editdir" -> cont TEditDir rest
                                        "deletedir" -> cont TDeleteDir rest
                                        "ls" -> cont TLS rest
                                        "cd" -> cont TCD rest
                                        "search" -> cont TSearch rest
                                        "ilike" -> cont TILike rest
                                        "exit" -> cont TExit rest
                                        _ -> cont (TVar s) rest
                          lexNum cs = case span isDigit cs of
                              (num,rest) -> cont (TNum num) rest
                          lexFlag cs = case span isFlag cs of
                              (s, rest) -> case L.toLower s of
                                        "-r" -> cont TRec rest
                                        _ -> cont (TVar s) rest
                              where isFlag c = c `elem` "-r"         
                          lexSym cs = case span isSymbol cs of
                              (">=", rest) -> cont TGte rest
                              ("<=", rest) -> cont TLte rest
                              ("=", rest) -> cont TEquals rest
                              ("(", rest) -> cont TOpen rest
                              (")", rest) -> cont TClose rest
                              ("/", rest) -> cont TSlash rest
                              (":", rest) -> cont TColon rest
                              (",", rest) -> cont TComma rest
                              (".", rest) -> cont TBack rest
                              ("!=", rest) -> cont TNequals rest
                              (">", rest) -> cont TGt rest
                              ("<", rest) -> cont TLt rest
                              (sym, rest) -> cont (TVar sym) rest
                              where isSymbol c = c `elem` "!<>=/,:()."  
                                           
exp_parse s = term s 1
comm_parse s = comm s 1
}
