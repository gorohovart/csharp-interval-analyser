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
    /// todo 
    vars, vars

let getProbsSum (values : List<VarValues * double>) : double = 
    values
    |> Seq.sumBy (fun (_,x) -> x)
        

let binaryOp left right (op : SyntaxToken) : VarValues = 
    /// todo
    let minG = 1
    let maxG = 1
    let maker operation = 
        (max(minG, Int32.MinValue),
            min(maxG, Int32.MaxValue))
        left
    match op.Text with
    | "+" -> maker (+)
    | "-" -> maker (-)
    | "/" -> maker (/)
    | "*" -> maker (*)
    | _ -> failwith "todo: unsupported binary operation"

let rec expression (expr : ExpressionSyntax) (vars : Dictionary<_,_>) : VarValues= 
//    let children = expr.ChildNodes()
//    if children.Count() > 1 then failwith "expression have more then one child"
//    let child = children.ToArray().[0]
    let outVars = ref null//<| new Dictionary<_,_>(vars)
    match expr with 
    | :? BinaryExpressionSyntax as binExpr -> 
        let left = expression binExpr.Left vars
        let right = expression binExpr.Right vars
        let operation = binExpr.OperatorToken
        
        binaryOp left right operation
    | :? InvocationExpressionSyntax as inv -> 
        Interval(MinValue, MaxValue)
    | _ -> 
        failwith "todo: unsupported expression type"

let blockWalker (block : ControlFlowBasicBlock) (vars : Dictionary<string,List<VarValues * double>>) =  // (semanticModel : SemanticModel) =
    let outVars = new Dictionary<_,_>(vars)
    for statement in block.Statements do
        //printfn "%s" <| statement.ToString()
        
        match statement.CSharpKind() with
        | SyntaxKind.VariableDeclaration -> 
            let variableDecl = (statement :?> LocalDeclarationStatementSyntax).Declaration
            let variables = variableDecl.Variables
            for variable in variables do
                let varName = variable.Identifier.Text
                if outVars.ContainsKey(varName) then failwith "Declaration of already declared variable"
                if variable.Initializer <> null
                then
                    let expr = variable.Initializer.Value
                    let value = expression expr vars
                    outVars.Add(varName, new List<_>([|value, 1.0|]))
                else
                    outVars.Add(varName, new List<_>([|Interval(Value 0,Value 0), 1.0|]))
        | SyntaxKind.ExpressionStatement -> 
            let exprStmt = statement :?> ExpressionStatementSyntax//AssignmentExpressionSyntax
            let expr = exprStmt.Expression
            match expr.CSharpKind() with 
            | SyntaxKind.SimpleAssignmentExpression -> 
                let binOp = expr :?> BinaryExpressionSyntax
                let varName = (binOp.Left :?> IdentifierNameSyntax).Identifier.Text
                if outVars.ContainsKey(varName)
                then
                    let probsSum = getProbsSum outVars.[varName]
                    let value = expression binOp.Right vars
                    outVars.Add(varName, new List<_>([|value, probsSum|]))
                else
                    failwith "assignment of undeclared variable"
            | _ -> failwith "todo unsupported type of expression"

        | SyntaxKind.ReturnStatement ->
            ()
        | _ -> //unsupported statement
            ()
    // undeclare vars defined in block
    let v = new Dictionary<_,_>()
    for pair in outVars do
        if vars.ContainsKey(pair.Key)
        then
            v.Add(pair.Key,  pair.Value)
    v

let supportedKinds =
    new Set<_>([| SyntaxKind.VariableDeclaration;
                  SyntaxKind.ExpressionStatement;
                  SyntaxKind.ReturnStatement;
                  //SyntaxKind.EqualsExpression,
                  //SyntaxKind.NotEqualsExpression,
                  SyntaxKind.IfStatement|])



let cfgWalker (cfg : ControlFlowGraph) (vars : Dictionary<string,List<VarValues * double>>) = 
    let blocksToProcess = new Queue<_>()
    blocksToProcess.Enqueue(cfg.FirstBlock,vars)

    let blocksInVars = new Dictionary<ControlFlowBasicBlock,List<Dictionary<string,List<_>>>>()

    while blocksToProcess.Count <> 0 do
        let currentBlock, vars = blocksToProcess.Dequeue()
        let outVars = blockWalker currentBlock vars

        let tryAddToQ block vars = 
            if blocksInVars.ContainsKey(block)
            then
                blocksInVars.[block].Add(vars)
            else
                let lst = new List<_>([|vars|])
                blocksInVars.Add(block, lst)
            if blocksInVars.[block].Count = cfg.InNodes.[block].Count
            then
                let lst = List.ofSeq <| blocksInVars.[block]
                let union = new Dictionary<_,_>(lst.[0])
                for vars in lst.[1..] do
                    for pair in vars do
                        let contains,value = union.TryGetValue(pair.Key)
                        if contains
                        then
                            value.AddRange(pair.Value)
                        else
                            union.Add(pair.Key, pair.Value)
                
                blocksToProcess.Enqueue(block, union) 

        if currentBlock.Successor <> null
        then 
            tryAddToQ currentBlock.Successor outVars
        if currentBlock.Condition <> null
        then
            let trueVars, falseVars = splitByCondition outVars currentBlock.Condition
            tryAddToQ currentBlock.TrueSuccessor trueVars
            tryAddToQ currentBlock.FalseSuccessor falseVars

let methodWalker (node : MethodDeclarationSyntax) i =
    let vars = new Dictionary<string,List<VarValues*double>>()
    for parameter in node.ParameterList.Parameters do
        let paramType = parameter.Type
        let paramName = parameter.Identifier
        if paramType.ToString() = "int"
        then
            vars.Add(paramName.Text, new List<_>([|Interval(MinValue, MaxValue), 1.0|]))
    
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