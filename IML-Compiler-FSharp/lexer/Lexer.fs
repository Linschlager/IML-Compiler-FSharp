namespace IML_Compiler_FSharp.lexer

open System
open IML_Compiler_FSharp.Types.Lexer

module Lexer =
    
    let addToList tokenList token =
        List.append tokenList [token]
    
    // TODO tbd
//    let rec readInputRec (codeInput: char[]) (pointer: int) (strAcc: string) (tokenListAcc: TokenList): TokenList =
//        if pointer >= codeInput.Length then
//            tokenListAcc
//        else
//            let c = codeInput.[pointer]
//            match strAcc with
//            | 0 ->
//                match c with
//                | _ when c = ' ' || c = char 13 || c = char 10 -> readInputRec codeInput (pointer+1) "" tokenListAcc // SKIP when whitespace
//                | '.' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.DOT })
//                | ';' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.SEMICOLON })
//                | ',' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.COMMA })
//                | '(' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.LPAREN })
//                | ')' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.RPAREN })
//                | '+' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.MONO_OPR (ADD PLUS) })
//                | '-' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.MONO_OPR (ADD MINUS) })
//                | '*' -> readInputRec codeInput (pointer+1) "" (addToList tokenListAcc { Terminal = Terminal.MULT_OPR MULTIPLY })
//                | '/' -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | '\\' -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | ':' -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | '>' -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | '<' -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | _ when (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z') -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | _ when Char.IsDigit c -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//                | _ -> failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
//            | "/" -> readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//            | _ when strAcc.StartsWith "//" ->
//                if c = char 13 then
//                    readInputRec codeInput (pointer+1) "" tokenListAcc
//                else
//                    readInputRec codeInput (pointer+1) (strAcc + string c) tokenListAcc
//            | _ when strAcc
//            | _ -> failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
    let readInput (codeInput: char[]) : TokenList =
        let mutable tokenList = []
        
        let addToList (token: Token) =
            tokenList <- List.append tokenList [token]
        
        let mutable state = Mode.Default
        let mutable start = 0
        let mutable pointer = 0
        while pointer < codeInput.Length do
            let c = codeInput.[pointer]
            match state with
            | Mode.Default ->
                start <- pointer
                match c with
                | '.' -> addToList Token.DOT
                | ';' -> addToList Token.SEMICOLON
                | ',' -> addToList Token.COMMA
                | '(' -> addToList Token.LPAREN
                | ')' -> addToList Token.RPAREN
                | '+' -> addToList (Token.MONO_OPR (ADD PLUS))
                | '-' -> addToList (Token.MONO_OPR (ADD MINUS))
                | '*' -> addToList (Token.MULT_OPR MULTIPLY)
                | '/' -> state <- Mode.Slash
                | ':' -> state <- Mode.Colon
                | '<' -> state <- Mode.LessThanSymbol
                | '>' -> state <- Mode.GreaterThanSymbol
                | _ when Char.IsLetter c || c = '_' -> state <- Mode.String
                | _ when Char.IsDigit c -> state <- Mode.Number
                | _ when Char.IsWhiteSpace c -> () // SKIP when whitespace
                | _ -> failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
            | Mode.String ->
                if not (Char.IsLetter c || Char.IsDigit c || c = '_') then
                    let string = new string (codeInput.[start..pointer-1])
                    addToList (LexerTypes.terminalFromKeyword string)
                    pointer <- pointer - 1
                    state <- Mode.Default
            | Mode.Number ->
                if not (Char.IsDigit c) then
                    let str = new string (codeInput.[start..pointer-1])
                    let number = int str
                    addToList (Token.LITERAL (INT number))
                    pointer <- pointer - 1
                    state <- Mode.Default
            | Mode.Colon ->
                if c = '=' then addToList Token.BECOMES
                else
                    addToList Token.COLON
                    pointer <- pointer - 1
                    state <- Mode.Default
            | Mode.LessThanSymbol ->
                if c = '=' then
                    addToList (Token.REL_OPR LESS_OR_EQUALS)
                else
                    addToList (Token.REL_OPR LESS_THAN)
                    pointer <- pointer - 1
                    state <- Mode.Default
            | Mode.GreaterThanSymbol ->
                if c = '=' then
                    addToList (Token.REL_OPR GREATER_OR_EQUALS)
                else
                    addToList (Token.REL_OPR GREATER_THAN)
                    pointer <- pointer - 1
                    state <- Mode.Default
            | Mode.Slash ->
                match c with
                | '/' -> state <- Mode.Comment
                | '\\' -> state <- Mode.SlashBackslash
                | '=' ->
                    addToList (Token.REL_OPR NOT_EQUALS)
                    state <- Mode.Default
                | _ -> failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
            | Mode.SlashBackslash ->
                if c = '?' then
                    addToList (Token.CONDITIONAL AND)
                    state <- Mode.Default
                else
                    printfn "Before: %s" (new string (codeInput.[start..pointer-1]))
                    failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
            | Mode.Backslash ->
                if c = '/' then
                    state <- Mode.BackslashSlash
                else
                    failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
                
            | Mode.BackslashSlash ->
                if c = '?' then
                    addToList (Token.CONDITIONAL OR)
                    state <- Mode.Default
                else
                    failwithf "Lexical error at %i: %c (%i)" pointer codeInput.[pointer] (int codeInput.[pointer])
            | Mode.Comment ->
                if c = char 13 then
                    printfn "Comment: %s" (new string (codeInput.[start..pointer-1]))
                    state <- Mode.Default
            
            pointer <- pointer + 1
        assert (state = Mode.Default)
        addToList Token.SENTINEL // Add Sentinel to the end of the list
        tokenList