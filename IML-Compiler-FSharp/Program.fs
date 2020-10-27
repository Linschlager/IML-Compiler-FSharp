namespace IML_Compiler_FSharp.lexer

open System.IO
open IML_Compiler_FSharp.parser

module Main =
    let readFile (filePath: string) =
        let pwd = Directory.GetCurrentDirectory ()
        let projectRoot = (Directory.GetParent pwd).Parent.Parent.FullName
        let relativePath = Path.Join (projectRoot, filePath)
        sprintf "%s " (File.ReadAllText relativePath) // Add sentinel whitespace at the end

    [<EntryPoint>]
    let main argv =
        let input = readFile argv.[0]
        printfn "%s" input
        let tokenList = Lexer.readInput (input.ToCharArray ())
        for el in tokenList do
            printf "%s " (el.ToString ())
            
        let parsed = Parser.program tokenList
        0 // return an integer exit code
