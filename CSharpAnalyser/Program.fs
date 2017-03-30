// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO
open System.Linq
open DiagnosticsTools
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild

open System.Reflection
open System.Text

let ParseStatement statement =
    let tree = CSharpSyntaxTree.ParseText("namespace A { class A() { A() { " + statement + " } } }")

    //printfn "%s" <| tree.ToString()
    tree.GetRoot()


let maker () = 
    let filePath = @"../../test.cs"
    //let workspace = MSBuildWorkspace.Create()
    
    
    //let solution = workspace.OpenSolutionAsync(solutionPath).Result

    

    
    let compilationUnit = CSharpSyntaxTree.ParseText(File.ReadAllText(filePath)).GetRoot()
    //let semanticModel = document.GetSemanticModelAsync().Result

    let methods = compilationUnit.DescendantNodes().OfType<MethodDeclarationSyntax>()
                
    methods 
    |> Seq.iteri(fun j singleMethod -> 
        Walker.methodWalker singleMethod (j.ToString()))
                



[<EntryPoint>]
let main argv = 
    try
        maker()
    with 
        | :? ReflectionTypeLoadException as ex -> 
            let sb = new StringBuilder()
            for exSub in ex.LoaderExceptions do
                sb.AppendLine(exSub.Message) |> ignore
                let exFileNotFound = exSub :?> FileNotFoundException
                if (exFileNotFound <> null)
                then
                    if(String.IsNullOrEmpty(exFileNotFound.FusionLog))
                    then
                        sb.AppendLine("Fusion Log:") |> ignore
                        sb.AppendLine(exFileNotFound.FusionLog) |> ignore
                sb.AppendLine() |> ignore
            let errorMessage = sb.ToString()
            printfn "%s" errorMessage
            //Display or log the error based on your application.



    


//    let node = 
//        ParseStatement <|
//            "{\n" +
//            "    var x\n" +
//            "    x = 7 + 1\n" +
//            "    x = x + 5\n" +
//            "    var y = 3\n" +
//            "    var z = new System.Collections.Generic.HashSet<int>()\n" +
//            "    if (x == 7) { a = 0; b = 0 } c = 0 }"
     
    

//    for block in cfg.BasicBlocks do
//        for statement in block.Statements do
//            printfn "%s" <| statement.ToString()
//            for n in statement.ChildNodes() do
//                ()
//                //Console.WriteLine("--->" + n.ToString())

    
    0 // return an integer exit code
