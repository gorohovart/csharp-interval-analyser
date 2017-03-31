using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using CSharpAnalyzers.ControlFlowGraph;

namespace CSharpAnalyzers
{
    internal sealed class MethodAnalyser
    {
        private static int minValue = Int32.MinValue;
        private static int maxValue = Int32.MaxValue;
        private DiagnosticDescriptor Rule;
        public List<Tuple<string, Location>> VariablesList = new List<Tuple<string, Location>>();

        public MethodAnalyser(IMethodSymbol node, SyntaxNode root, SemanticModel model, DiagnosticDescriptor rule)
        {
            Rule = rule;
            var vars = new Dictionary<string, List<Interval>>();
            foreach (var parameter in node.Parameters)
            {
                var paramType = parameter.Type;
                var paramName = parameter.Name;
                if(paramType.ToString() == "int")
                    vars.Add(paramName, new List<Interval>{new Interval(minValue, maxValue)});
            }
                

            //let cfg = blockWalker node.Body vars     
            var cfg = ControlFlowGraph.ControlFlowGraph.Create(root);


            CFGWalker(cfg, vars, model);
            //File.WriteAllText(@"C:\Code\roslyn\src\Samples\CSharp\Analyzers\" + new Random().Next(1000) + "cfg.dot", cfg.ToGraph());
        }

        private List<Interval> BinaryOp(List<Interval> left, List<Interval> right, SyntaxToken op)
        {
            var result = new List<Interval>();
            foreach(var leftInterval in left)
            foreach (var rightInterval in right)
            {
                int[] G;
                int minG, maxG;
                switch (op.Text)
                {
                    case "+":
                        G = new[]
                        {
                            leftInterval.Low + rightInterval.Low,
                            leftInterval.High + rightInterval.Low,
                            leftInterval.Low + rightInterval.High,
                            leftInterval.High + rightInterval.High
                        };

                        //        let prob = (left.Prob * right.Prob) / Prob

                        minG = G.Min();
                        maxG = G.Max();

                        result.Add(new Interval(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
                        break;
                    case "-":
                        G = new[]
                        {
                            leftInterval.Low - rightInterval.Low,
                            leftInterval.High - rightInterval.Low,
                            leftInterval.Low - rightInterval.High,
                            leftInterval.High - rightInterval.High
                        };

                        //        let prob = (left.Prob * right.Prob) / Prob

                        minG = G.Min();
                        maxG = G.Max();

                        result.Add(new Interval(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
                        break;
                    case "/":
                        G = new[]
                        {
                            leftInterval.Low / rightInterval.Low,
                            leftInterval.High / rightInterval.Low,
                            leftInterval.Low / rightInterval.High,
                            leftInterval.High / rightInterval.High
                        };

                        //        let prob = (left.Prob * right.Prob) / Prob

                        minG = G.Min();
                        maxG = G.Max();

                        result.Add(new Interval(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
                        break;
                    case "*":
                        G = new[]
                        {
                            leftInterval.Low * rightInterval.Low,
                            leftInterval.High * rightInterval.Low,
                            leftInterval.Low * rightInterval.High,
                            leftInterval.High * rightInterval.High
                        };

                        //        let prob = (left.Prob * right.Prob) / Prob

                        minG = G.Min();
                        maxG = G.Max();

                        result.Add(new Interval(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
                        break;
                    default:
                        break;
                }
            }
            return result;
        }

        private List<Interval> Expression(ExpressionSyntax expr, Dictionary<string, List<Interval>> vars)
        {
            //var outVars;
            var binExpr = expr as BinaryExpressionSyntax;
            var inv = expr as InvocationExpressionSyntax;
            var literal = expr as LiteralExpressionSyntax;
            var ident = expr as IdentifierNameSyntax;
            List<Interval> result;
            if (binExpr != null)
            { 
                var left = Expression(binExpr.Left, vars);
                var right = Expression(binExpr.Right, vars);
                var operation = binExpr.OperatorToken;
                result = BinaryOp(left, right, operation);
            }
            else if(inv != null)
            {
                result = new List<Interval> {new Interval(minValue, maxValue)};
            }
            else if (literal != null)
            {
                var value = Int32.Parse(literal.Token.Text);
                result = new List<Interval> {new Interval(value, value)};
            }
            else if (ident != null)
            {
                var identName = ident.Identifier.Text;
                result = vars[identName];
            }
            else
                throw new Exception("todo: unsupported expression type");
            return result;
        }

        private Dictionary<string, List<Interval>> BlockWalker(ControlFlowBasicBlock block,
            Dictionary<string, List<Interval>> vars, SemanticModel semanticModel)
        {
            var outVars = new Dictionary<string, List<Interval>>(vars);
            foreach (var statement in block.Statements)
            {
                switch (statement.Kind())
                {
                    case SyntaxKind.VariableDeclaration:
                        var variableDecl = (VariableDeclarationSyntax) statement;
                        var typeOfVars = semanticModel.GetSymbolInfo(variableDecl.Type);
                        var nameOfType = typeOfVars.Symbol.ToString();
                        VariablesList.Add(new Tuple<string, Location>(nameOfType, variableDecl.GetLocation()));
                        //printfn "Type: %s" nameOfType
                        if (nameOfType != "int" && nameOfType != "Int32") break;
                        var variables = variableDecl.Variables;
                        foreach (var variable in variables)
                        {
                            var varName = variable.Identifier.Text;
                            if (outVars.ContainsKey(varName))
                                throw new Exception("Declaration of already declared variable");
                            if (variable.Initializer != null)
                            {
                                var initExpression = variable.Initializer.Value;
                                var value = Expression(initExpression, outVars);
                                outVars.Add(varName, value);
                            }
                            else
                                outVars.Add(varName, new List<Interval> {new Interval(0, 0)});
                        }
                        break;
                    case SyntaxKind.ExpressionStatement:
                        var exprStmt = (ExpressionStatementSyntax) statement;
                        var expr = exprStmt.Expression;
                        switch (expr.Kind())
                        {
                            case SyntaxKind.SimpleAssignmentExpression:

                                var binOp = (AssignmentExpressionSyntax) expr;
                                var varName = ((IdentifierNameSyntax) binOp.Left).Identifier.Text;
                                if (outVars.ContainsKey(varName))
                                {
                                    //                    var probsSum = getProbsSum outVars.[varName]
                                    var value = Expression(binOp.Right, outVars);
                                    //                    var x = value |> List.map (fun x -> Interval(x.Low, x.High, x.Prob * probsSum))
                                    outVars.Remove(varName);
                                    outVars.Add(varName, value);
                                }
                                else
                                    throw new Exception("assignment of undeclared variable");
                                break;
                            case SyntaxKind.InvocationExpression:
                                break;
                            default:
                                throw new Exception("todo unsupported type of expression");
                        }
                        break;
                    case (SyntaxKind.ReturnStatement):
                        break;
                        throw new Exception("todo unsupported RETURN expression");
                    default:
                        throw new Exception("todo unsupported type of expression");
                    // undeclare vars defined in block
                }
            }
            var v = new Dictionary<string, List<Interval>>();
            foreach (var pair in outVars)
            {
                if (vars.ContainsKey(pair.Key))
                    v.Add(pair.Key, pair.Value);
            }
            return v;
        }

        private Tuple<Dictionary<string,List<Interval>>, Dictionary<string, List<Interval>>> SplitByCondition(Dictionary<string, List<Interval>> vars, ExpressionSyntax cond)
        {
            var conditionExpression = cond as BinaryExpressionSyntax;
            var unaryExpression = cond as PrefixUnaryExpressionSyntax;
            if (conditionExpression != null)
            {
                var leftExpr = conditionExpression.Left;
                var rightExpr = conditionExpression.Right;
                switch (conditionExpression.Kind())
                {
                    case SyntaxKind.EqualsExpression:
                        throw new Exception("unsupported type on binary condition");
                    case SyntaxKind.LessThanExpression:
                        throw new Exception("unsupported type on binary condition");
                    case SyntaxKind.LessThanOrEqualExpression:
                        throw new Exception("unsupported type on binary condition");
                    case SyntaxKind.GreaterThanExpression:
                        throw new Exception("unsupported type on binary condition");
                    case SyntaxKind.GreaterThanOrEqualExpression:
                        throw new Exception("unsupported type on binary condition");
                    case SyntaxKind.LogicalAndExpression:
                        throw new Exception("unsupported type on binary condition");
                    case SyntaxKind.LogicalOrExpression:
                        throw new Exception("unsupported type on binary condition");
                    default: throw new Exception("unsupported type on binary condition");
                        
                }
            }
            else if (unaryExpression != null)
            {
                switch (unaryExpression.Kind())
                {
                    case SyntaxKind.LogicalNotExpression:
                        throw new Exception("unsupported type on unary condition");
                    default:
                        throw new Exception("unsupported type on unary condition");
                }
            }
            else
            {
                throw new Exception("unsupported type on condition");
            }
            // todo
            return new Tuple<Dictionary<string, List<Interval>>, Dictionary<string, List<Interval>>>(vars, vars);
        }

        private void tryAddToQ(ControlFlowGraph.ControlFlowGraph cfg, ControlFlowBasicBlock block , Dictionary<string, List<Interval>> vars,
            Dictionary<ControlFlowBasicBlock, List<Dictionary<string, List<Interval>>>> blocksInVars,
            Queue<Tuple<ControlFlowBasicBlock, Dictionary<string,List<Interval>>>> blocksToProcess)
        {
            if (blocksInVars.ContainsKey(block))

                blocksInVars[block].Add(vars);
            else
            {
                var list = new List<Dictionary<string, List<Interval>>> {vars};
                blocksInVars.Add(block, list);
            }
            if(blocksInVars[block].Count == cfg.InNodes[block].Count)
            {
                var union = new Dictionary<string, List<Interval>>(blocksInVars[block][0]);
                for(int i = 1; i < blocksInVars[block].Count; i++)
                {
                    foreach(var pair in blocksInVars[block][i])
                    {
                        List<Interval> value;
                        var contains = union.TryGetValue(pair.Key,out value);
                        if (contains)
                        {
                            union.Remove(pair.Key);
                            var res = new List<Interval>(
                                value.Concat(pair.Value).GroupBy(x => new Tuple<int, int>(x.High, x.Low))
                                    .Select(x => new Interval(x.Key.Item1, x.Key.Item2)));
                            union.Add(pair.Key, res);
                        }
                        else
                            union.Add(pair.Key, pair.Value);
                    }
}
                blocksToProcess.Enqueue(new Tuple<ControlFlowBasicBlock, Dictionary<string, List<Interval>>>(block, union));

            }
        }


        private void CFGWalker(ControlFlowGraph.ControlFlowGraph cfg, Dictionary<string, List<Interval>> vars,
            SemanticModel semanticModel)
        {
            var blocksToProcess = new Queue<Tuple<ControlFlowBasicBlock, Dictionary<string, List<Interval>>>>();
            blocksToProcess.Enqueue(new Tuple<ControlFlowBasicBlock, Dictionary<string, List<Interval>>>(
                cfg.FirstBlock, vars));

            var blocksInVars = new Dictionary<ControlFlowBasicBlock, List<Dictionary<string, List<Interval>>>>();

            while (blocksToProcess.Count != 0)
            {
                var inr = blocksToProcess.Dequeue();
                var currentBlock = inr.Item1;
                var currentBlockVars = inr.Item2;
                var outVars = BlockWalker(currentBlock, currentBlockVars, semanticModel);


                if (currentBlock.Successor != null)
                    tryAddToQ(cfg, currentBlock.Successor, outVars, blocksInVars, blocksToProcess);

                var cond = currentBlock.Condition;
                if (cond != null)
                {
                    var innr = SplitByCondition(outVars, cond);
                    var trueVars = innr.Item1;
                    var falseVars = innr.Item2;
                    tryAddToQ(cfg, currentBlock.TrueSuccessor, trueVars, blocksInVars, blocksToProcess);
                    tryAddToQ(cfg, currentBlock.FalseSuccessor, falseVars, blocksInVars, blocksToProcess);
                }
            }
        }

        public struct Interval
        {
            public int Low;
            public int High;

            public Interval(int low, int high)
            {
                Low = low;
                High = high;
            }
        }

        public void ShowVariablesList(CodeBlockAnalysisContext context)
        {
            foreach (var parameter in VariablesList)
            {
                var diagnostic = Diagnostic.Create(Rule, parameter.Item2, parameter.Item1, parameter.Item1);
                context.ReportDiagnostic(diagnostic);
            }
        }
    }
}