using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using CSharpAnalyzers.ControlFlowGraph;
using MyAnalyser.Variables;

namespace CSharpAnalyzers
{
    internal sealed class MethodAnalyser
    {
        private static int minValue = Int32.MinValue;
        private static int maxValue = Int32.MaxValue;
        private readonly DiagnosticDescriptor Rule;
        public List<Tuple<string, Location>> VariablesList = new List<Tuple<string, Location>>();

        public MethodAnalyser(IMethodSymbol node, SyntaxNode root, SemanticModel model, DiagnosticDescriptor rule)
        {
            Rule = rule;
            var vars = new Variables();
            foreach (var parameter in node.Parameters)
            {
                var paramType = parameter.Type;
                var paramName = parameter.Name;
                if (paramType.ToString() == "int")
                    vars.Values.Add(paramName, new PrimitiveValue("", minValue, maxValue));
            }
                

            //let cfg = blockWalker node.Body vars     
            var cfg = ControlFlowGraph.ControlFlowGraph.Create(root);


            CFGWalker(cfg, vars, model);
            //File.WriteAllText(@"C:\Code\roslyn\src\Samples\CSharp\Analyzers\" + new Random().Next(1000) + "cfg.dot", cfg.ToGraph());
        }

        private static bool TryGetType(Type type, string typeName, out string outType, out bool isArray)
        {
            string[] alowedTypes = {"int", "int16"};
            isArray = type.GetTypeInfo().IsArray;
            if (type.GetTypeInfo().IsPrimitive)
            {
                if (alowedTypes.Contains(typeName))
                {
                    outType = typeName;
                    return true;
                }
                outType = "";
                return false;
            }
            outType = typeName;
            return false;
        }

        private PrimitiveValue BinaryOp(PrimitiveValue left, PrimitiveValue right, SyntaxToken op)
        {
            var result = new PrimitiveValue("");
            foreach(var leftInterval in left.Intervals)
            foreach (var rightInterval in right.Intervals)
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

                        result.Intervals.Add(new Interval<int>(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
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

                        result.Intervals.Add(new Interval<int>(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
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

                        result.Intervals.Add(new Interval<int>(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
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

                        result.Intervals.Add(new Interval<int>(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
                        break;
                    default:
                        break;
                }
            }
            return result;
        }

        private Primitive Expression(ExpressionSyntax expr, Variables vars)
        {
            var binExpr = expr as BinaryExpressionSyntax;
            var invocationExpression = expr as InvocationExpressionSyntax;
            var literal = expr as LiteralExpressionSyntax;
            var identifierName = expr as IdentifierNameSyntax;
            var initializerExpression = expr as InitializerExpressionSyntax;
            Primitive result;
            if (binExpr != null)
            { 
                var left = (PrimitiveValue)Expression(binExpr.Left, vars);
                var right = (PrimitiveValue)Expression(binExpr.Right, vars);
                var operation = binExpr.OperatorToken;
                result = BinaryOp(left, right, operation);
            }
            else if(invocationExpression != null)
            {
                result = new PrimitiveValue("") { Intervals = new List<Interval<int>> { new Interval<int>(minValue, maxValue) } };
            }
            else if (literal != null)
            {
                var value = Int32.Parse(literal.Token.Text);
                result = new PrimitiveValue("") {Intervals = new List<Interval<int>>{new Interval<int>(value, value)}};
            }
            else if (identifierName != null)
            {
                var identName = identifierName.Identifier.Text;
                result = vars.Values[identName];
            }
            else if (initializerExpression != null)
            {
                var lengthOfArray = initializerExpression.Expressions.Count;
                var elemsOfArray = initializerExpression.Expressions;

                result = new PrimitiveArray("", lengthOfArray);
                for (var i = 0; i < lengthOfArray; i++)
                {
                    var value = VariableInitializer(elemsOfArray[i], vars);
                    ((PrimitiveArray)result).Elements[i] = value;
                }
            }
            else
                throw new Exception("todo: unsupported expression type");
            return result;
        }

        //private void UnaryExpression(ExpressionSyntax expr, Variables vars)

        private void StatementExpression(ExpressionSyntax expr, Variables vars)
        {
            var invocationExpr = expr as InvocationExpressionSyntax;
            var assignmentExpr = expr as AssignmentExpressionSyntax;
            var postUnaryExpr = expr as PostfixUnaryExpressionSyntax;
            var preUnaryExpr = expr as PrefixUnaryExpressionSyntax;
            var objCreationExpr = expr as ObjectCreationExpressionSyntax;
            if (assignmentExpr != null)
            {
                if (assignmentExpr.Left.IsKind(SyntaxKind.IdentifierName))
                {   // variable = ...
                    var varName = ((IdentifierNameSyntax)assignmentExpr.Left).Identifier.Text;
                    var newVar = Expression(assignmentExpr.Right, vars);
                    vars.Values.Remove(varName);
                    vars.Values.Add(varName, newVar);
                }
                else if (assignmentExpr.Left.IsKind(SyntaxKind.ElementAccessExpression))
                {
                    // todo: check partial access
                    var elementAccess = (ElementAccessExpressionSyntax) assignmentExpr.Left;
                    var arrayName = ((IdentifierNameSyntax)elementAccess.Expression).Identifier.Text;
                    foreach (var arg in elementAccess.ArgumentList.Arguments)
                    {
                        var res = Expression(arg.Expression, vars);
                        //todo: check for out of array bounds
                    }
                    
                    var exprValue = Expression(assignmentExpr.Right, vars);
                    // todo: check for out of type bounds
                    var arrayVar = (PrimitiveArray)vars.Values[arrayName];

                }
            }
            else if (invocationExpr != null)
            {
                throw new Exception("todo unsupported invocation expression");
            }   
            else if (postUnaryExpr != null)
            {
                throw new Exception("todo unsupported postUnary expression");
            }
            else if (preUnaryExpr != null)
            {
                throw new Exception("todo unsupported preUnary expression");
            }
            else if (objCreationExpr != null)
            {
                throw new Exception("todo unsupported objCreation expression");
            }
            
        }

        private Primitive VariableInitializer(ExpressionSyntax expr, Variables vars)
        {
            Primitive newVar;
            if (expr is InitializerExpressionSyntax)
            {
                var initializerExpression = (InitializerExpressionSyntax) expr;
                var lengthOfArray = initializerExpression.Expressions.Count;
                var elemsOfArray = initializerExpression.Expressions;

                newVar = new PrimitiveArray("", lengthOfArray);
                for (var i = 0; i < lengthOfArray; i++)
                {
                    var value = VariableInitializer(elemsOfArray[i], vars);
                    ((PrimitiveArray)newVar).Elements[i] = value;
                }
            }
            else
            {
                newVar = Expression(expr, vars);
            }
            return newVar;
        }

        private void DeclarationStatement(LocalDeclarationStatementSyntax declaration,
            Variables vars, SemanticModel semanticModel)
        {   
            var variableDecl = declaration.Declaration;
            var modifiers = declaration.Modifiers;
            var typeOfVars = semanticModel.GetSymbolInfo(variableDecl.Type);
            var nameOfType = typeOfVars.Symbol.ToString();

            VariablesList.Add(new Tuple<string, Location>(nameOfType, variableDecl.GetLocation()));
            //printfn "Type: %s" nameOfType
            //if (nameOfType != "int" && nameOfType != "Int32") break;
            bool isConst = modifiers.Select(x => x.Text).Contains("const");
            string outName;
            bool isArray;
            if (!TryGetType(typeOfVars.GetType(), nameOfType, out outName, out isArray)) return;

            foreach (var variableDeclarator in variableDecl.Variables)
            {
                var varName = variableDeclarator.Identifier.Text;
                if (vars.Values.ContainsKey(varName))
                    throw new Exception("Declaration of already declared variable");

                Primitive newVar;
                if (variableDeclarator.Initializer != null)
                {
                    newVar = VariableInitializer(variableDeclarator.Initializer.Value, vars);
                }
                else if (isArray)
                {
                    newVar = new PrimitiveArray(nameOfType);
                }
                else
                {
                    newVar = new PrimitiveValue(nameOfType);
                }
                vars.Values.Add(varName, newVar);
            }
        }

        private void ReturnStatement(ReturnStatementSyntax expr, Variables vars)
        {
            throw new Exception("todo unsupported RETURN expression");
        }

        private Variables BlockWalker(ControlFlowBasicBlock block,
            Variables vars, SemanticModel semanticModel)
        {
            var outVars = new Variables(vars);
            foreach (var statement in block.Statements)
            {
                if (statement.IsKind(SyntaxKind.LocalDeclarationStatement))
                {
                    var variableDecl = (LocalDeclarationStatementSyntax) statement;
                    DeclarationStatement(variableDecl, outVars, semanticModel);
                }
                else if (statement.IsKind(SyntaxKind.ExpressionStatement))
                {
                    var exprStmt = (ExpressionStatementSyntax) statement;
                    var expr = exprStmt.Expression;
                    StatementExpression(expr, outVars);
                }
                else if (statement.IsKind(SyntaxKind.ReturnStatement))
                    ReturnStatement((ReturnStatementSyntax) statement, outVars);
                else
                    throw new Exception("todo unsupported type of expression");
            }
            //var v = new Variables();
            //foreach (var pair in outVars)
            //{
            //    if (vars.ContainsVariable(pair.Key))
            //        v.Add(pair.Key, pair.Value);
            //}
            return outVars;
        }

        private Tuple<Variables, Variables> SplitByCondition(Variables vars, ExpressionSyntax cond)
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
            return new Tuple<Variables, Variables>(vars, vars);
        }

        private void tryAddToQ(ControlFlowGraph.ControlFlowGraph cfg, ControlFlowBasicBlock block , Variables vars,
            Dictionary<ControlFlowBasicBlock, List<Variables>> blocksInVars,
            Queue<Tuple<ControlFlowBasicBlock, Variables>> blocksToProcess)
        {
            if (blocksInVars.ContainsKey(block))

                blocksInVars[block].Add(vars);
            else
            {
                var list = new List<Variables> {vars};
                blocksInVars.Add(block, list);
            }
            if(blocksInVars[block].Count == cfg.InNodes[block].Count)
            {
                var union = new Variables(blocksInVars[block][0]);
                for(var i = 1; i < blocksInVars[block].Count; i++)
                {
                    foreach(var pair in blocksInVars[block][i].Values)
                    {
                        Primitive value;
                        if (union.Values.TryGetValue(pair.Key, out value))
                        {
                            union.Values.Remove(pair.Key);
                            var res = value.Concat(pair.Value);
                            union.Values.Add(pair.Key, res);
                        }
                        else
                            union.Values.Add(pair.Key, pair.Value);
                    }
                }
                blocksToProcess.Enqueue(new Tuple<ControlFlowBasicBlock, Variables>(block, union));
            }
        }


        private void CFGWalker(ControlFlowGraph.ControlFlowGraph cfg, Variables vars,
            SemanticModel semanticModel)
        {
            var blocksToProcess = new Queue<Tuple<ControlFlowBasicBlock, Variables>>();
            blocksToProcess.Enqueue(new Tuple<ControlFlowBasicBlock, Variables>(
                cfg.FirstBlock, vars));

            var blocksInVars = new Dictionary<ControlFlowBasicBlock, List<Variables>>();

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