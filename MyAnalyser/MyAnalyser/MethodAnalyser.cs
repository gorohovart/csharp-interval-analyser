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
using MyAnalyser;
using MyAnalyser.VarStructures;

namespace CSharpAnalyzers
{
    internal sealed class MethodAnalyser
    {
        private static int minValue = Int32.MinValue;
        private static int maxValue = Int32.MaxValue;
        private readonly DiagnosticDescriptor Rule;

        public ErrorNotifier ErrorNotifier = new ErrorNotifier();

        public MethodAnalyser(IMethodSymbol node, SyntaxNode root, SemanticModel model, DiagnosticDescriptor rule)
        {
            Rule = rule;
            var vars = new Variables();
            foreach (var parameter in node.Parameters)
            {
                var paramType = parameter.Type;
                var paramName = parameter.Name;
                if (paramType.ToString() == "int")
                    vars.Values.Add(paramName, new PrimitiveValue(minValue, maxValue));
            }

            //let cfg = blockWalker node.Body vars     
            var cfg = ControlFlowGraph.ControlFlowGraph.Create(root);

            CFGWalker(cfg, vars, model);
            //File.WriteAllText(@"C:\Code\roslyn\src\Samples\CSharp\Analyzers\" + new Random().Next(1000) + "cfg.dot", cfg.ToGraph());
        }

        private Interval WideIntervals(Interval interval1, Interval interval2)
        {
            const int k = 100; // -k to k

            var x1 =
                interval1.Low <= interval2.Low
                    ? interval1.Low
                    : interval2.Low >= -1 * k ? interval2.Low : Int32.MinValue;
            var x2 =
                interval1.High >= interval2.High
                    ? interval1.High
                    : interval2.High <= k ? interval2.High : Int32.MaxValue;
            return Interval.Get(x1,x2);
        }

        private Primitive WidePrimitives(Primitive prim1, Primitive prim2)
        {
            var value1 = prim1 as PrimitiveValue;
            var value2 = prim2 as PrimitiveValue;
            if (value1 != null && value2 != null)
            {
                var interval1 = Interval.Get(value1.GetLow(), value1.GetHigh());
                var interval2 = Interval.Get(value2.GetLow(), value2.GetHigh());

                return new PrimitiveValue(WideIntervals(interval1, interval2));
            }
            var arr1 = prim1 as PrimitiveArray;
            var arr2 = prim2 as PrimitiveArray;
            if (arr1 != null && arr2 != null)
            {
                return arr2;
            }
            throw new Exception("Type casting exeption while widening");
        }

        
        private Variables Widening(Variables vars1, Variables vars2)
        {
            if (vars1 == null) return vars2;
            var wided = new Variables(vars1);
            //var list = vars1.Values.ToList();
            foreach (var kvp in vars2.Values)
            {
                Primitive outVal;
                if (vars1.Values.TryGetValue(kvp.Key, out outVal))
                {
                    wided.Values.Remove(kvp.Key);
                    wided.Values.Add(kvp.Key,WidePrimitives(outVal, kvp.Value));
                }
                else
                {
                    wided.Values.Add(kvp.Key, kvp.Value);
                }
            }

            return wided;
        }

        private Tuple<Variables, Variables> SplitByCondition(Variables vars, ExpressionSyntax cond)
        {
            return new Tuple<Variables, Variables>(vars, vars);
            //var conditionExpression = cond as BinaryExpressionSyntax;
            //var unaryExpression = cond as PrefixUnaryExpressionSyntax;
            //if (conditionExpression != null)
            //{
            //    var leftExpr = conditionExpression.Left;
            //    var rightExpr = conditionExpression.Right;
            //    switch (conditionExpression.Kind())
            //    {
            //        case SyntaxKind.EqualsExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        case SyntaxKind.LessThanExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        case SyntaxKind.LessThanOrEqualExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        case SyntaxKind.GreaterThanExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        case SyntaxKind.GreaterThanOrEqualExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        case SyntaxKind.LogicalAndExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        case SyntaxKind.LogicalOrExpression:
            //            throw new Exception("unsupported type on binary condition");
            //        default: throw new Exception("unsupported type on binary condition");

            //    }
            //}
            //else if (unaryExpression != null)
            //{
            //    switch (unaryExpression.Kind())
            //    {
            //        case SyntaxKind.LogicalNotExpression:
            //            throw new Exception("unsupported type on unary condition");
            //        default:
            //            throw new Exception("unsupported type on unary condition");
            //    }
            //}
            //else
            //{
            //    throw new Exception("unsupported type on condition");
            //}
            //// todo
            //return new Tuple<Variables, Variables>(vars, vars);
        }

        private Variables UnionVars(Variables vars1, Variables vars2)
        {
            var union = new Variables(vars1);
            foreach (var pair in vars2.Values)
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
            return union;
        }

        private void addToQueue(ControlFlowBasicBlock block , Variables vars,
            Dictionary<ControlFlowBasicBlock, Variables> blocksInVars,
            Queue<Tuple<ControlFlowBasicBlock, Variables>> blocksToProcess)
        {
            if (blocksInVars.ContainsKey(block))

                blocksInVars[block] = UnionVars(vars, blocksInVars[block]);
                
            else
            {
                blocksInVars.Add(block, vars);
            }
            blocksToProcess.Enqueue(new Tuple<ControlFlowBasicBlock, Variables>(block, blocksInVars[block]));
        }

        private void CFGWalker(ControlFlowGraph.ControlFlowGraph cfg, Variables vars,
            SemanticModel semanticModel)
        {
            var blocksToProcess = new Queue<Tuple<ControlFlowBasicBlock, Variables>>();
            blocksToProcess.Enqueue(new Tuple<ControlFlowBasicBlock, Variables>(
                cfg.FirstBlock, vars));

            var blocksInVars = new Dictionary<ControlFlowBasicBlock, Variables>();

            while (blocksToProcess.Count != 0)
            {
                var inr = blocksToProcess.Dequeue();
                var currentBlock = inr.Item1;
                var currentBlockInVars = inr.Item2;
                Variables outVars;
                
                if (!BlockAnalyser.GetInstance(currentBlock, semanticModel, ErrorNotifier)
                                  .TryToRun((x,y) => 
                                            Widening(x,y), currentBlockInVars, out outVars)) continue;

                if (currentBlock.Successor != null)
                    addToQueue(currentBlock.Successor, outVars, blocksInVars, blocksToProcess);

                var cond = currentBlock.Condition;
                if (cond == null) continue;
                var innr = SplitByCondition(outVars, cond);
                var trueVars = innr.Item1;
                var falseVars = innr.Item2;
                addToQueue(currentBlock.TrueSuccessor, trueVars, blocksInVars, blocksToProcess);
                addToQueue(currentBlock.FalseSuccessor, falseVars, blocksInVars, blocksToProcess);
            }
        }

        public void ShowErrors(CodeBlockAnalysisContext context)
        {
           foreach (var errors in ErrorNotifier.Errors.Values)
            {
                foreach (var error in errors)
                {
                    var diagnostic = Diagnostic.Create(Rule, error.Location, error.Text, error.Text);
                    context.ReportDiagnostic(diagnostic);
                }
            }
        }
    }
}