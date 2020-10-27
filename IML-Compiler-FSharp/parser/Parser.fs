namespace IML_Compiler_FSharp.parser

open IML_Compiler_FSharp.Types.Lexer

module Parser =
    let consume expectedTerminal (tokenList: TokenList): TokenList =
        match tokenList with
        | head::restOfList -> 
            if head = expectedTerminal then
                restOfList
            else
                failwithf "Unexpected Terminal. Expected '%s' found %s" (expectedTerminal.ToString ()) (head.ToString ())                
        | _ ->  failwith "Error. TokenList is empty"
        
    let identifier (tokenList: TokenList): TokenList =
        printf "IDENTIFIER"
        match tokenList with
        | (Token.IDENT s)::tail ->
            printfn " %s" s
            tail
        | head::_ ->
            failwithf "Unexpected Terminal %s. Expected Identifier" (head.ToString ())
        | _ -> failwith "" 
    
    let type' (tokenList: TokenList): TokenList =
        printf "TYPE"
        match tokenList with
        | Token.BOOL::tail ->
            printfn "=BOOL" 
            tail
        | (Token.INT_TYPE i)::tail ->
            printfn "=%s" (i.ToString ())
            tail
        | (Token.IDENT i)::tail ->
            printfn "=%s" (i.ToString ())
            tail
        | _ -> failwith "Unexpected Terminal. Expected Identifier"   
    
    let typedIdentifier (tokenList: TokenList): TokenList =
        tokenList
        |> identifier
        |> consume Token.COLON
        |> type'
    
    let progParam (tokenList: TokenList): TokenList =
        let woFlowMode =
            match tokenList with
            | (Token.FLOW_MODE f)::rest -> rest
            | _ -> tokenList
        let woChangeMode =
            match woFlowMode with
            | (Token.CHANGE_MODE c)::rest -> rest
            | _ -> tokenList
        woChangeMode    
        |> typedIdentifier
    
    let rec progParamListGroup (tokenList: TokenList): TokenList =
        let newList = progParam tokenList
        if newList.Head = Token.COMMA then
            progParamListGroup newList
        else
            newList
    
    let progParamListOpt (tokenList: TokenList): TokenList =
        match tokenList.Head with
        | Token.FLOW_MODE _ -> progParamListGroup tokenList
        | Token.CHANGE_MODE _ -> progParamListGroup tokenList
        | Token.IDENT _ -> progParamListGroup tokenList
        | _ -> tokenList
        
    
    let progParamList (tokenList: TokenList): TokenList =
        printfn "PROG_PARAM_LIST"
        tokenList
        |> consume Token.LPAREN
        |> progParamListOpt
        |> consume Token.RPAREN
        
    let param (tokenList: TokenList): TokenList =
        printf "PARAM"
        let woFlowMode =
            match tokenList with
            | (Token.FLOW_MODE m)::tail ->
                printf "-FLOWMODE"
                tail
            | _ -> tokenList
        let woMechMode =
            match woFlowMode with
            | (Token.MECH_MODE m)::tail ->
                printf "-MECHMODE"
                tail
            | _ -> tokenList
        let woChangeMode =
            match woMechMode with
            | (Token.CHANGE_MODE m)::tail ->
                printf "-CHANGEMODE"
                tail
            | _ -> tokenList
        printfn "\n"
        woChangeMode
        |> typedIdentifier
        
    let rec paramGroup (tokenList: TokenList): TokenList =
        match param tokenList with
        | Token.COMMA::tail -> paramGroup tail
        | _ -> tokenList
    
    let paramListOpt (tokenList: TokenList): TokenList =
        match tokenList.Head with
        | Token.FLOW_MODE _ -> paramGroup tokenList
        | Token.MECH_MODE _ -> paramGroup tokenList
        | Token.CHANGE_MODE _ -> paramGroup tokenList
        | Token.IDENT _ -> paramGroup tokenList
        | _ -> tokenList
    
    let paramList (tokenList: TokenList): TokenList =
        tokenList
        |> consume Token.LPAREN
        |> paramListOpt
        |> consume Token.RPAREN
    
    let stoDecl (tokenList: TokenList): TokenList =
        let woChangeMode =
            match tokenList with
            | Token.CHANGE_MODE c::tail ->
                tail
            | _ -> tokenList
        woChangeMode
        |> typedIdentifier
        
    let rec expression (tokenList: TokenList): TokenList = 
        tokenList // TODO implement
    
    let rec expressionGroup (tokenList: TokenList): TokenList =
        match expression tokenList with
        | Token.COMMA::tail -> expressionGroup tail
        | _ as newList -> newList 
    
    let rec expressionGroupOpt (tokenList: TokenList): TokenList =
        match tokenList.Head with
        | Token.IDENT i -> expressionGroup tokenList
        | Token.LITERAL l -> expressionGroup tokenList
        | Token.MONO_OPR o -> expressionGroup tokenList
        | Token.LPAREN -> expressionGroup tokenList
        | _ -> tokenList
    
    let expressionList (tokenList: TokenList): TokenList =
        tokenList
        |> consume Token.LPAREN
        |> expressionGroupOpt
        |> consume Token.RPAREN
    
    let globInitsOpt (tokenList: TokenList): TokenList =
        tokenList
    
    let rec cpsCommand (tokenList: TokenList): TokenList =
        match tokenList with
        | Token.SKIP::tail -> tail
        | Token.IF::_ ->
            tokenList
            |> consume Token.IF
            |> expression
            |> consume Token.THEN
            |> cpsCommand
            |> fun list ->
                match list with
                | Token.ELSE::tail -> cpsCommand tail
                | _ -> list
            |> consume Token.ENDIF
        | Token.WHILE::_ ->
            tokenList
            |> consume Token.WHILE
            |> expression
            |> consume Token.DO
            |> cpsCommand
            |> consume Token.ENDWHILE
        | Token.CALL::_ ->
            tokenList
            |> consume Token.CALL
            |> identifier
            |> expressionList
            |> globInitsOpt
        | Token.DEBUG_MODE m::_ ->
            match m with
            | DEBUG_IN ->
                tokenList
                |> consume (Token.DEBUG_MODE DEBUG_IN)
                |> expression
            | DEBUG_OUT ->
                tokenList
                |> consume (Token.DEBUG_MODE DEBUG_OUT)
                |> expression
        | _ -> 
            tokenList
            |> expression
            |> consume Token.BECOMES
            |> expression // TODO or record
        
    let globImp (tokenList: TokenList): TokenList =
        let woFlowMode =
            match tokenList with
            | (Token.FLOW_MODE f)::rest -> rest
            | _ -> tokenList
        let woChangeMode =
            match woFlowMode with
            | (Token.CHANGE_MODE c)::rest -> rest
            | _ -> tokenList
        woChangeMode    
        |> identifier
        
    let rec globalImpsGroup (tokenList: TokenList): TokenList =
        match globImp tokenList with
        | Token.COMMA::tail -> globalImpsGroup tail
        | _ as newList -> newList

    let globalImpsOpt (tokenList: TokenList): TokenList =
        match tokenList with
        | Token.GLOBAL::tail -> globalImpsGroup tail
        | _ -> tokenList

    let rec localCpsStoDeclGroup (tokenList: TokenList): TokenList =
        match stoDecl tokenList with
        | Token.SEMICOLON::tail -> localCpsStoDeclGroup tail
        | _ as newList -> newList 
    
    let localCpsStoDeclOpt (tokenList: TokenList): TokenList =
        match tokenList with
        | Token.LOCAL::tail -> localCpsStoDeclGroup tail
        | _ -> tokenList

    let funDecl (tokenList: TokenList): TokenList =
        tokenList
        |> consume Token.FUN
        |> identifier
        |> paramList
        |> consume Token.RETURNS
        |> stoDecl
        |> globalImpsOpt
        |> localCpsStoDeclOpt
        |> consume Token.DO
        |> cpsCommand
        |> consume Token.END_FUN
        
    let procDecl (tokenList: TokenList): TokenList =
        tokenList
        |> consume Token.PROC
        |> identifier
        |> paramList
        |> globalImpsOpt
        |> localCpsStoDeclOpt
        |> consume Token.DO
        |> cpsCommand
        |> consume Token.END_PROC
        
    let rec typedIdentifiers tokenList =
        match typedIdentifier tokenList with
        | Token.COMMA::tail -> typedIdentifiers tail
        | _ as newList -> newList
    
    let recordParamList (tokenList: TokenList): TokenList =
        tokenList
        |> consume Token.LPAREN
        |> typedIdentifiers
        |> consume Token.RPAREN
    
    let recordShapeDecl (tokenList: TokenList): TokenList =
        tokenList
        |> consume Token.RECORD
        |> identifier
        |> recordParamList
    
    let decl (tokenList: TokenList): TokenList =
        match tokenList.Head with
        | Token.CHANGE_MODE _ ->
            printfn "DECL -> STO_DECL"
            stoDecl tokenList
        | Token.FUN ->
            printfn "DECL -> FUN_DECL"
            funDecl tokenList
        | Token.PROC ->
            printfn "DECL -> PROC_DECL"
            procDecl tokenList
        | Token.RECORD ->
            printfn "DECL -> RECORD_SHAPE_DECL"
            recordShapeDecl tokenList
        | _ -> failwith "Invalid Declaration"
    
    let rec declGroup (tokenList: TokenList): TokenList =
        match decl tokenList with
        | Token.SEMICOLON :: tail -> declGroup tail
        | _ as newList -> newList 
    
    let compositionDeclaration tokenList =
        printfn "CPS_DECL"
        tokenList
        |> declGroup
    
    let globalPart (tokenList: TokenList): TokenList =
        printfn "GLOBAL"
        tokenList
        |> consume Token.GLOBAL
        |> compositionDeclaration
    
    let globalPartOpt (tokenList: TokenList): TokenList =
        match tokenList.Head with
        | Token.GLOBAL -> globalPart tokenList
        | _ -> tokenList
    
    let compositionCommand tokenList =
        tokenList
    
    let program tokenList =
        printfn "\n\n\nPROGRAM"
        tokenList
        |> consume Token.PROGRAM
        |> identifier
        |> progParamList
        |> globalPartOpt
        |> consume Token.DO
        |> compositionCommand
        |> consume Token.END_PROGRAM
        |> consume Token.SENTINEL