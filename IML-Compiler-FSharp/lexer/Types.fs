namespace IML_Compiler_FSharp.lexer

open IML_Compiler_FSharp.Types.Lexer

module LexerTypes =
    let terminalFromKeyword keywordStr =
        match keywordStr with
        | "program" -> Token.PROGRAM
        | "endprogram" -> Token.END_PROGRAM
        | "global" -> Token.GLOBAL
        | "local" -> Token.LOCAL
        | "if" -> Token.IF
        | "else" -> Token.ELSE
        | "endif" -> Token.ENDIF
        | "call" -> Token.CALL
        | "bool" -> Token.BOOL
        | "true" -> Token.BOOL_VALUE TRUE
        | "false" -> Token.BOOL_VALUE FALSE
        | "int32" -> Token.INT_TYPE INT_32
        | "int64" -> Token.INT_TYPE INT_64
        | "int1024" -> Token.INT_TYPE INT_1024
        | "do" -> Token.DO
        | "fun" -> Token.FUN
        | "endfun" -> Token.END_FUN
        | "proc" -> Token.PROC
        | "skip" -> Token.SKIP
        | "endproc" -> Token.END_PROC
        | "const" -> Token.CHANGE_MODE CONST
        | "var" -> Token.CHANGE_MODE VAR
        | "ref" -> Token.MECH_MODE REF
        | "copy" -> Token.MECH_MODE COPY
        | "in" -> Token.FLOW_MODE IN
        | "out" -> Token.FLOW_MODE OUT
        | "inout" -> Token.FLOW_MODE INOUT
        | "divE" -> Token.MULT_OPR (DIVIDE DIV_E)
        | "divT" -> Token.MULT_OPR (DIVIDE DIV_T)
        | "divF" -> Token.MULT_OPR (DIVIDE DIV_F)
        | "modE" -> Token.MULT_OPR (DIVIDE MOD_E)
        | "modT" -> Token.MULT_OPR (DIVIDE MOD_T)
        | "modF" -> Token.MULT_OPR (DIVIDE MOD_F)
        | "not" -> Token.MONO_OPR NOT
        | "record" -> Token.RECORD
        | "returns" -> Token.RETURNS
        | "debugout" -> Token.DEBUG_MODE DEBUG_OUT
        | "debugin" -> Token.DEBUG_MODE DEBUG_IN
        | _ -> Token.IDENT keywordStr