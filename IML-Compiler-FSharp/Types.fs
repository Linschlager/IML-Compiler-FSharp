namespace IML_Compiler_FSharp.Types

module Lexer =
    type BOOL_VALUES =
        | TRUE
        | FALSE

    type INT_TYPES =
        | INT_32
        | INT_64
        | INT_1024

    type LITERAL =
        | BOOL of BOOL_VALUES
        | INT of int

    type DEBUG_MODE =
        | DEBUG_IN
        | DEBUG_OUT

    type FLOW_MODES =
        | IN
        | OUT
        | INOUT

    type MECH_MODES =
        | COPY
        | REF

    type CHANGE_MODES =
        | CONST
        | VAR

    type REL_OPR =
        | EQUALS
        | NOT_EQUALS
        | LESS_THAN
        | GREATER_THAN
        | LESS_OR_EQUALS
        | GREATER_OR_EQUALS
        
    type ADD_OPR =
        | PLUS
        | MINUS

    type DIVISION_OPR =
        | DIV_E
        | MOD_E
        | DIV_F
        | MOD_F
        | DIV_T
        | MOD_T

    type MULT_OPR =
        | MULTIPLY
        | DIVIDE of DIVISION_OPR

    type MONO_OPR =
        | ADD of ADD_OPR
        | NOT
        
    type CONDITIONAL_OPR =
        | AND
        | OR

    [<RequireQualifiedAccess>]
    type Token =
        | PROGRAM
        | END_PROGRAM
        | BECOMES
        | WHILE
        | ENDWHILE
        | GLOBAL
        | LOCAL
        | SKIP
        | IF
        | THEN
        | ELSE
        | ENDIF
        | CALL
        | LPAREN
        | RPAREN
        | COLON
        | SEMICOLON
        | COMMA
        | RECORD
        | DOT
        | DO
        | FUN
        | END_FUN
        | PROC
        | END_PROC
        | BOOL
        | INIT
        | RETURNS
        | CONDITIONAL of CONDITIONAL_OPR
        | IDENT of string
        | LITERAL of LITERAL
        | DEBUG_MODE of DEBUG_MODE
        | MECH_MODE of MECH_MODES
        | CHANGE_MODE of CHANGE_MODES
        | BOOL_VALUE of BOOL_VALUES
        | FLOW_MODE of FLOW_MODES
        | INT_TYPE of INT_TYPES
        | MONO_OPR of MONO_OPR
        | REL_OPR of REL_OPR
        | MULT_OPR of MULT_OPR
        | SENTINEL

    [<RequireQualifiedAccess>]
    type Mode =
        | Default
        | String
        | Number
        | LessThanSymbol
        | GreaterThanSymbol
        | Colon
        | Backslash
        | BackslashSlash
        | Slash
        | SlashBackslash
        | Comment

    type TokenList = Token list 