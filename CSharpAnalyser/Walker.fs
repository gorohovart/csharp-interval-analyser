module Walker
open System
open System.Collections.Generic
open System.IO
open System.Linq

open DiagnosticsTools

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.MSBuild
open Intervals



let invocation inv = 
    ()

let splitByCondition vars condition = 
    ()

let expression (expr : ExpressionStatementSyntax) = 
    let children = expr.ChildNodes()
    if children.Count() > 1 then failwith "expression have more then one child"
    let child = children.ToArray().[0]
    match child.CSharpKind() with 
    | SyntaxKind.InvocationExpression -> 
        let inv = child :?> InvocationExpressionSyntax
        invocation inv
        ()
    | SyntaxKind.SimpleAssignmentExpression -> 
        ()

let blockWalker (block : ControlFlowBasicBlock) vars =  // (semanticModel : SemanticModel) =
    let outVars = new Dictionary<_,_>()
    for statement in block.Statements do
        //printfn "%s" <| statement.ToString()
        
        match statement.CSharpKind() with
        | SyntaxKind.VariableDeclaration -> 
            let variableDecl = statement :> SyntaxNode :?> LocalDeclarationStatementSyntax
            //let //VariableDeclarationSyntax
            //let x = semanticModel.GetSymbolInfo(variableDecl)//.GetSymbolInfo(variableDecl.Declaration.Type).Symbol
            //variableDecl.Variables
//            printfn "%A" <| x//variableDecl.GetType()
//            for variable in variableDecl.Variables do
//                printfn "variable: %A" <| variable
            ()
        | SyntaxKind.ExpressionStatement -> 
            let expr = statement :?> ExpressionStatementSyntax//AssignmentExpressionSyntax
            expression expr
            ()
        | SyntaxKind.IfStatement ->
            let ifNode = statement :?> IfStatementSyntax
            ifNode.Condition
            ifNode.Statement
            ifNode.Else
            ()
        | SyntaxKind.ReturnStatement ->
            ()
        | SyntaxKind.EqualsExpression -> 
            ()
        | SyntaxKind.NotEqualsExpression -> 
            ()
        | _ -> //unsupported statement
            ()


let supportedKinds =
    new Set<_>([|
        SyntaxKind.VariableDeclaration;
        SyntaxKind.ExpressionStatement;
        SyntaxKind.ReturnStatement;
        //SyntaxKind.EqualsExpression,
        //SyntaxKind.NotEqualsExpression,
        SyntaxKind.IfStatement|])

let cfgWalker (cfg : ControlFlowGraph) vars = 
    let blocks = new Queue<_>()
    blocks.Enqueue(cfg.FirstBlock,vars)
    while blocks.Count <> 0 do
        let currentBlock, vars = blocks.Dequeue()
        let outVars = blockWalker currentBlock vars

        if currentBlock.Successor <> null
        then blocks.Enqueue(currentBlock.Successor, outVars)
        
        if currentBlock.Condition <> null
        then
            let trueVars, falseVars = splitByCondition outVars currentBlock.Condition
            blocks.Enqueue(currentBlock.TrueSuccessor, trueVars)
            blocks.Enqueue(currentBlock.FalseSuccessor, falseVars)

let methodWalker (node : MethodDeclarationSyntax) i =
    let vars = new Dictionary<_,_>()
    for parameter in node.ParameterList.Parameters do
        let paramType = parameter.Type
        let paramName = parameter.Identifier
        if paramType.ToString() = "int"
        then
            vars.Add(paramName.Text, Interval(MinValue, MaxValue))
    
    //let cfg = blockWalker node.Body vars     
    let cfg = ControlFlowGraph.Create(node.Body :> SyntaxNode)


    cfgWalker cfg vars
    File.WriteAllText(i + "cfg.dot", cfg.ToGraph())

//    if supportedKinds.Contains(node.CSharpKind())
//    then
//        ()
 
//(new CSharpSyntaxWalker()).



type CodeBlockWalker(vars : Dictionary<string, VarValues>) =
    inherit CSharpSyntaxWalker()
    
    override this.VisitMethodDeclaration(node : MethodDeclarationSyntax ) = 
        let outVars = new Dictionary<_,_>()
        for parameter in node.ParameterList.Parameters do
            let paramType = parameter.Type
            let paramName = parameter.Identifier
            if paramType.ToString() = "int"
            then
                vars.Add(paramName.Text, Interval(MinValue, MaxValue))
        ()

    override this.VisitVariableDeclaration(node : VariableDeclarationSyntax ) = 
        ()

type MethodWalker(vars : Dictionary<string, VarValues>) =
    inherit CSharpSyntaxWalker()
    
    override this.VisitMethodDeclaration(node : MethodDeclarationSyntax ) = 
        let outVars = new Dictionary<_,_>()
        for parameter in node.ParameterList.Parameters do
            let paramType = parameter.Type
            let paramName = parameter.Identifier
            if paramType.ToString() = "int"
            then
                vars.Add(paramName.Text, Interval(MinValue, MaxValue))
        ()

    override this.VisitVariableDeclaration(node : VariableDeclarationSyntax ) = 
        ()