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
                    vars.Values.Add(paramName, new PrimitiveValue("", minValue, maxValue));
            }

            //let cfg = blockWalker node.Body vars     
            var cfg = ControlFlowGraph.ControlFlowGraph.Create(root);

            CFGWalker(cfg, vars, model);
            //File.WriteAllText(@"C:\Code\roslyn\src\Samples\CSharp\Analyzers\" + new Random().Next(1000) + "cfg.dot", cfg.ToGraph());
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
                var outVars = new BlockAnalyser(currentBlock, currentBlockVars, semanticModel, ErrorNotifier).Run();


                if (currentBlock.Successor != null)
                    tryAddToQ(cfg, currentBlock.Successor, outVars, blocksInVars, blocksToProcess);

                var cond = currentBlock.Condition;
                if (cond == null) continue;
                var innr = SplitByCondition(outVars, cond);
                var trueVars = innr.Item1;
                var falseVars = innr.Item2;
                tryAddToQ(cfg, currentBlock.TrueSuccessor, trueVars, blocksInVars, blocksToProcess);
                tryAddToQ(cfg, currentBlock.FalseSuccessor, falseVars, blocksInVars, blocksToProcess);
            }
        }

        public void ShowErrors(CodeBlockAnalysisContext context)
        {
            foreach (var error in ErrorNotifier.Errors.Values)
            {
                var diagnostic = Diagnostic.Create(Rule, error.Location, error.Text);
                context.ReportDiagnostic(diagnostic);
            }
        }
    }
}