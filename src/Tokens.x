{
module Lexer where
}

%wrapper "basic"

$white = [\ ]
$digit = 0-9
$alpha = [a-zA-Z]


tokens :-
    $white+                  ;
    \t+                           ;
    [\n \;]+                      { \s -> TokenNewLine}
    int                          { \s -> TokenIntType }
    if                            { \s -> TokenIf }
    else                          { \s -> TokenElse }
    while                         { \s -> TokenWhile}
    Write                         { \s -> TokenWrite}
    ReadLn                         { \s -> TokenReadLn}
    WriteLn                         { \s -> TokenWriteLn}
    "<"                            { \s -> TokenLess }
    "="                            { \s -> TokenAssign }
    "=="                            { \s -> TokenCompare }
    "+"                            { \s -> TokenPlus }
    "-"                            { \s -> TokenMinus }
    "{"                            { \s -> TokenOpenBracket }
    "}"                            { \s -> TokenCloseBracket }
    "("                            { \s -> TokenOpenParenthesis }
    ")"                            { \s -> TokenCloseParenthesis }
    $digit+                       { \s -> TokenInt (read s) }
    $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

-- The token type:
data Token = TokenVar
           | TokenIntType
           | TokenInt Int
           | TokenSym String
           | TokenAssign
           | TokenPlus
           | TokenMinus
           | TokenNewLine
           | TokenIf
           | TokenElse
           | TokenWhile
           | TokenWrite
           | TokenReadLn
           | TokenWriteLn
           | TokenLess
           | TokenCompare
           | TokenOpenBracket
           | TokenCloseBracket
           | TokenOpenParenthesis
           | TokenCloseParenthesis
           deriving (Eq,Show)

scanTokens = alexScanTokens

}