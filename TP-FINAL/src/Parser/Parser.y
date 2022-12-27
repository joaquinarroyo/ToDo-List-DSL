{
module Parser.Parser where
import Filter.AST
import Command.AST 
import Structures.Task
import Structures.Folder
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
    '/'         { TSlash }
    ','         { TComma }
    ".."        { TBack }
    NAME        { TName }
    DESCRIPTION { TDescription }
    COMPLETED   { TCompleted }
    PRIORITY    { TPriority }
    TIMESTAMP   { TTimestamp }
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
    NEWDIR      { TNewDir }
    EDITDIR     { TEditDir }
    DELETEDIR   { TDeleteDir }
    LS          { TLS }
    CD          { TCD }
    SEARCH      { TSearch }

%right VAR
%left '=' '>' '<'
%left AND OR
%right NOT

%%

Comm    : NEWTASK '(' VAR ',' VAR ',' NUM ',' Date ')' { NewTask $3 $5 (read $7) $9 }
        | DELETETASK VAR                               { DeleteTask $2 }
        | EDITTASK Field VAR                           { EditTask $2 $3 }
        | EDITTASK COMPLETED Bool                      { EditTaskB $3 }
        | EDITTASK PRIORITY NUM                        { EditTaskP (read $3) }
        | EDITTASK TIMESTAMP Date                      { EditTaskT $3 }
        | NEWDIR VAR                                   { NewDir $2 }
        | EDITDIR VAR                                  { EditDir $2 }
        | DELETEDIR VAR                                { DeleteDir $2 }
        | LS                                           { LS }
        | CD Route                                     { CD $2 }
        | SEARCH Exp                                   { Search $2 }

Route   : VAR '/' Route { Route $1 $3 }
        | VAR           { Route $1 Empty } 
        | ".."          { Back }

Exp     : Field '=' VAR              { FieldEq $1 $3 }
        | COMPLETED '=' Bool         { FieldEqB $3 }
        | PRIORITY '=' NUM           { FieldEqP (read $3) }
        | PRIORITY '>' NUM           { FieldGtP (read $3) }
        | PRIORITY '<' NUM           { FieldLtP (read $3) }
        | TIMESTAMP '=' Date         { FieldEqT $3 }
        | TIMESTAMP '>' Date         { FieldGtT $3 }
        | TIMESTAMP '<' Date         { FieldLtT $3 }
        | Exp AND Exp                { And $1 $3 }
        | Exp OR Exp                 { Or $1 $3 }
        | NOT Exp                    { Not $2 }
        | '(' Exp ')'                { $2 }
        
Field   : NAME                       { Name }
        | DESCRIPTION                { Description }

Date    : NUM '/' NUM '/' NUM  NUM ':' NUM  { ($1 ++ "-" ++ $3 ++ "-" ++ $5 ++ " " ++ $6 ++ ":" ++ $8) }
        | NUM '/' NUM '/' NUM               { ($1 ++ "-" ++ $3 ++ "-" ++ $5) }


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
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

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
                | TTrue
                | TFalse
                | TGt
                | TLt
                | TNewTask
                | TDeleteTask
                | TEditTask
                | TNewDir
                | TEditDir
                | TDeleteDir
                | TLS
                | TCD
                | TSearch
                | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                          | isDigit c -> lexNum (c:cs)
                    (':':cs) -> cont TColon cs
                    ('(':cs) -> cont TOpen cs
                    (')':cs) -> cont TClose cs
                    ('=':cs) -> cont TEquals cs
                    ('<':cs) -> cont TLt cs
                    ('>':cs) -> cont TGt cs
                    ('/':cs) -> cont TSlash cs
                    (',':cs) -> cont TComma cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                              ("name",rest) -> cont TName rest
                              ("description",rest) -> cont TDescription rest
                              ("completed", rest) -> cont TCompleted rest
                              ("priority", rest) -> cont TPriority rest
                              ("timestamp", rest) -> cont TTimestamp rest
                              ("and",rest) -> cont TAnd rest
                              ("or",rest) -> cont TOr rest
                              ("not",rest) -> cont TNot rest
                              ("true",rest) -> cont TTrue rest
                              ("false",rest) -> cont TFalse rest
                              ("newtask",rest) -> cont TNewTask rest
                              ("deletetask",rest) -> cont TDeleteTask rest
                              ("edittask",rest) -> cont TEditTask rest
                              ("newdir",rest) -> cont TNewDir rest
                              ("editdir",rest) -> cont TEditDir rest
                              ("deletedir",rest) -> cont TDeleteDir rest
                              ("ls",rest) -> cont TLS rest
                              ("cd",rest) -> cont TCD rest
                              ("search",rest) -> cont TSearch rest
                              ("..",rest) -> cont TBack rest
                              (var,rest) -> cont (TVar var) rest
                          lexNum cs = case span isDigit cs of
                              (num,rest) -> cont (TNum num) rest
                                           
exp_parse s = term s 1
comm_parse s = comm s 1
}
