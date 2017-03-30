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

let minValue = Int32.MinValue
let maxValue = Int32.MaxValue

let invocation inv = 
    ()

//let getProbsSum (values : seq<Interval>) : double = 
//    values
//    |> Seq.sumBy (fun x -> x.Prob)
        

let binaryOp (left : Interval list) (right : Interval list) (op : SyntaxToken) (vars : Dictionary<_,_>) : Interval list = 
//    let Prob = getProbsSum left * getProbsSum right
    
    let maker operation (left : Interval) (right : Interval) = 
        let G = [operation left.Low right.Low;
                 operation left.Low right.High;
                 operation left.High right.Low;
                 operation left.High right.High]
        
//        let prob = (left.Prob * right.Prob) / Prob

        let minG = G |> List.min
        let maxG = G |> List.max

        Interval(max minG minValue, min maxG maxValue(*, prob*))

    [ for leftInterval in left do
          for rightInterval in right do
              match op.Text with
              | "+" -> yield maker (+) leftInterval rightInterval
              | "-" -> yield maker (-) leftInterval rightInterval
              | "/" -> failwith "todo: unsupported \"/\" operation" //yield maker (/) leftInterval rightInterval
              | "*" -> yield maker (*) leftInterval rightInterval
              | _ -> failwith "todo: unsupported binary operation" ]

let rec expression (expr : ExpressionSyntax) (vars : Dictionary<string,Interval list>) : Interval list = 
//    let children = expr.ChildNodes()
//    if children.Count() > 1 then failwith "expression have more then one child"
//    let child = children.ToArray().[0]
    let outVars = ref null//<| new Dictionary<_,_>(vars)
    match expr with 
    | :? BinaryExpressionSyntax as binExpr -> 
        let left = expression binExpr.Left vars
        let right = expression binExpr.Right vars
        let operation = binExpr.OperatorToken
        
        binaryOp left right operation vars
    | :? InvocationExpressionSyntax as inv -> 
        [Interval(minValue, maxValue(*, 1.0*))]
    | :? LiteralExpressionSyntax as literal -> 
        let value = Int32.Parse literal.Token.Text
        [Interval(value, value(*, 1.0*))]
    | :? IdentifierNameSyntax as ident -> 
        let identName = ident.Identifier.Text
        vars.[identName] |> List.ofSeq
    | _ -> failwith "todo: unsupported expression type"

let condition (cond : ExpressionSyntax) (vars : Dictionary<string,Interval list>) =
    ()

let blockWalker (block : ControlFlowBasicBlock) (vars : Dictionary<string,Interval list>) =  // (semanticModel : SemanticModel) =
    let outVars = new Dictionary<_,_>(vars)
    for statement in block.Statements do
        //printfn "%s" <| statement.ToString()
        
        match statement.CSharpKind() with
        | SyntaxKind.VariableDeclaration -> 
            let variableDecl = (statement :?> LocalDeclarationStatementSyntax).Declaration
            let typeOfVars = variableDecl.Type.GetText().ToString()
            let variables = variableDecl.Variables
            for variable in variables do
                let varName = variable.Identifier.Text
                if outVars.ContainsKey(varName) then failwith "Declaration of already declared variable"
                if variable.Initializer <> null
                then
                    let expr = variable.Initializer.Value
                    let value = expression expr vars
                    outVars.Add(varName, value)
                else
                    outVars.Add(varName, [Interval(0,0(*,1.0*))])
        | SyntaxKind.ExpressionStatement -> 
            let exprStmt = statement :?> ExpressionStatementSyntax
            let expr = exprStmt.Expression 
            match expr.CSharpKind() with 
            | SyntaxKind.SimpleAssignmentExpression -> 
                let binOp = expr :?> BinaryExpressionSyntax
                let varName = (binOp.Left :?> IdentifierNameSyntax).Identifier.Text
                if outVars.ContainsKey(varName)
                then
//                    let probsSum = getProbsSum outVars.[varName]
                    let value = expression binOp.Right vars
//                    let x = value |> List.map (fun x -> Interval(x.Low, x.High, x.Prob * probsSum))
                    outVars.Add(varName, (*x*)value)
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

let splitByCondition (vars : Dictionary<string,Interval list>) (cond : ExpressionSyntax) =
    match cond with
    | :? BinaryExpressionSyntax as conditionExpression ->
        let leftExpr = conditionExpression.Left
        let rightExpr = conditionExpression.Right
        match conditionExpression.CSharpKind() with
        | SyntaxKind.LessThanExpression -> ()
        | SyntaxKind.LessThanOrEqualExpression -> ()
        | SyntaxKind.GreaterThanExpression -> ()
        | SyntaxKind.GreaterThanOrEqualExpression -> ()
        | SyntaxKind.LogicalAndExpression -> ()
        | SyntaxKind.LogicalOrExpression -> ()
        | _ -> failwith "unsupported type on binary condition"
    | :? PrefixUnaryExpressionSyntax as unaryExpression ->
        match unaryExpression.CSharpKind() with
        | SyntaxKind.LogicalNotExpression -> ()
        | _ -> failwith "unsupported type on unary condition"
    | _ -> failwith "unsupported type on condition"
    vars, vars

let cfgWalker (cfg : ControlFlowGraph) (vars : Dictionary<string,Interval list>) = 
    let blocksToProcess = new Queue<_>()
    blocksToProcess.Enqueue(cfg.FirstBlock,vars)

    let blocksInVars = new Dictionary<ControlFlowBasicBlock,List<Dictionary<string, Interval list>>>()

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
                            union.Remove(pair.Key) |> ignore
                            let res = 
                                (value @ pair.Value)
                                |> List.groupBy(fun x -> x.High, x.Low)
                                |> List.map(fun ((low, high),lst) ->Interval(low, high(*, getProbsSum lst*)))
                            union.Add(pair.Key, res)
                        else
                            union.Add(pair.Key, pair.Value)
                
                blocksToProcess.Enqueue(block, union) 

        if currentBlock.Successor <> null
        then 
            tryAddToQ currentBlock.Successor outVars

        let cond = currentBlock.Condition
        if cond <> null
        then
            let trueVars, falseVars = splitByCondition outVars cond
            tryAddToQ currentBlock.TrueSuccessor trueVars
            tryAddToQ currentBlock.FalseSuccessor falseVars

let methodWalker (node : MethodDeclarationSyntax) i =
    let vars = new Dictionary<string,Interval list>()
    for parameter in node.ParameterList.Parameters do
        let paramType = parameter.Type
        let paramName = parameter.Identifier
        if paramType.ToString() = "int"
        then
            vars.Add(paramName.Text, [Interval(minValue, maxValue(*, 1.0*))])
    
    //let cfg = blockWalker node.Body vars     
    let cfg = ControlFlowGraph.Create(node.Body :> SyntaxNode)


    cfgWalker cfg vars
    File.WriteAllText(i + "cfg.dot", cfg.ToGraph())

//    if supportedKinds.Contains(node.CSharpKind())
//    then
//        ()
 
//(new CSharpSyntaxWalker()).



type CodeBlockWalker(vars : Dictionary<string, Interval>) =
    inherit CSharpSyntaxWalker()
    
    override this.VisitMethodDeclaration(node : MethodDeclarationSyntax ) = 
        let outVars = new Dictionary<_,_>()
        for parameter in node.ParameterList.Parameters do
            let paramType = parameter.Type
            let paramName = parameter.Identifier
            if paramType.ToString() = "int"
            then
                vars.Add(paramName.Text, Interval(minValue, maxValue(*, 1.0*)))
        ()

    override this.VisitVariableDeclaration(node : VariableDeclarationSyntax ) = 
        ()

type MethodWalker(vars : Dictionary<string, Interval>) =
    inherit CSharpSyntaxWalker()
    
    override this.VisitMethodDeclaration(node : MethodDeclarationSyntax ) = 
        let outVars = new Dictionary<_,_>()
        for parameter in node.ParameterList.Parameters do
            let paramType = parameter.Type
            let paramName = parameter.Identifier
            if paramType.ToString() = "int"
            then
                vars.Add(paramName.Text, Interval(minValue, maxValue(*, 1.0*)))
        ()

    override this.VisitVariableDeclaration(node : VariableDeclarationSyntax ) = 
        ()