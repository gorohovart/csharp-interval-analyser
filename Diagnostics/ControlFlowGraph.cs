using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Immutable;

namespace DiagnosticsTools
{
    public class ControlFlowGraph
    {
        private ControlFlowGraph(IList<ControlFlowBasicBlock> basicBlocks, ControlFlowBasicBlock firstBlock)
        {
            BasicBlocks = basicBlocks;
            FirstBlock = firstBlock;
        }

        public ControlFlowBasicBlock FirstBlock;
        public IList<ControlFlowBasicBlock> BasicBlocks { get; private set; }

        private String GetCause(ControlFlowBasicBlock predecessor)
        {
            return predecessor.Terminator != null ? Statement(predecessor.Terminator) : "";
        }

        public String ToGraph()
        {
            StringBuilder sb = new StringBuilder();

            sb.AppendLine("digraph finite_state_machine {");
            sb.AppendLine("  rankdir=TB;");

            foreach (ControlFlowBasicBlock basicBlock in BasicBlocks)
            {
                sb.AppendLine("  node [shape = circle, label = \"" + Statements(basicBlock.Statements) + "\"]; B" + BasicBlocks.IndexOf(basicBlock) + ";");
            }

            foreach (ControlFlowBasicBlock predecessor in BasicBlocks)
            {
                if (predecessor.TrueSuccessor != null)
                {
                    var cause = GetCause(predecessor);
                    sb.AppendLine(" B" + BasicBlocks.IndexOf(predecessor) + " -> B" + BasicBlocks.IndexOf(predecessor.TrueSuccessor) + " [label = \"" + cause + "\ntrue" + "\"]");
                }
                if (predecessor.FalseSuccessor != null)
                {
                    var cause = GetCause(predecessor);
                    sb.AppendLine(" B" + BasicBlocks.IndexOf(predecessor) + " -> B" + BasicBlocks.IndexOf(predecessor.FalseSuccessor) + " [label = \"" + cause + "\nfalse" + "\"]");
                }
                if (predecessor.Successor != null)
                {
                    sb.AppendLine(" B" + BasicBlocks.IndexOf(predecessor) + " -> B" + BasicBlocks.IndexOf(predecessor.Successor) + " [label = \"\"]");
                }

            }

            sb.AppendLine("}");

            return sb.ToString();
        }

        private static String Statements(IList<SyntaxNode> statements)
        {
            StringBuilder sb = new StringBuilder();

            foreach (SyntaxNode statement in statements)
            {
                sb.AppendLine(Statement(statement));
            }

            return sb.ToString();
        }

        private static String Statement(SyntaxNode statement)
        {
            const int max = 500;
            
            var ifStatement = statement as IfStatementSyntax;
            String text = (ifStatement != null ? ifStatement.Condition : statement).ToString().Replace("\"", "\\\"").Replace("  ", " ");
            String result = text.Length > max ? text.Substring(0, max - 3) + "..." : text;
            return " " + result + " ";
        }

        public static ControlFlowGraph Create(SyntaxNode node)
        {
            return new ControlFlowGraphBuilder().Build(node);
        }

        private class ControlFlowGraphBuilder : SyntaxWalker
        {
            private readonly ImmutableHashSet<SyntaxKind> supportedKinds = new[]
            {
                SyntaxKind.VariableDeclaration,
                SyntaxKind.ExpressionStatement,
                SyntaxKind.ReturnStatement,
                //SyntaxKind.EqualsExpression,
                //SyntaxKind.NotEqualsExpression,
                //SyntaxKind.IfStatement
            }.ToImmutableHashSet();

            public ControlFlowBasicBlock FirstBlock;
            private ControlFlowBasicBlock currentBasicBlock;
            private HashSet<ControlFlowBasicBlock> basicBlocks = new HashSet<ControlFlowBasicBlock>();

            public ControlFlowGraph Build(SyntaxNode node)
            {
                FirstBlock = new ControlFlowBasicBlock();
                currentBasicBlock = FirstBlock;
                Visit(node);

                basicBlocks.Add(currentBasicBlock);

                return new ControlFlowGraph(basicBlocks.ToList(), FirstBlock);
            }

            public override void Visit(SyntaxNode node)
            {
                // FIXME How comes a node can be null here?
                if (node != null)
                {
                    if (supportedKinds.Contains(node.CSharpKind()))
                    {
                        currentBasicBlock.Statements.Add(node);
                    }
                    else if (node.IsKind(SyntaxKind.IfStatement))
                    {
                        var ifNode = (IfStatementSyntax)node;

                        var conditionBasicBlock = currentBasicBlock;
                        var ifTrueBasicBlock = new ControlFlowBasicBlock();
                        var afterIfBasicBlock = new ControlFlowBasicBlock();

                        conditionBasicBlock.Terminator = ifNode;
                        //Visit(ifNode.Condition);
                        conditionBasicBlock.Condition = ifNode.Condition;
                        //ifTrueBasicBlock.Successor = afterIfBasicBlock;
                        SetCurrentBasicBlock(ifTrueBasicBlock);
                        Visit(ifNode.Statement);
                        currentBasicBlock.Successor = afterIfBasicBlock;
                        if (ifNode.Else != null)
                        {
                            var elseBasicBlock = new ControlFlowBasicBlock();
                            //elseBasicBlock.Successor = afterIfBasicBlock;
                            SetCurrentBasicBlock(elseBasicBlock);
                            Visit(ifNode.Else);
                            currentBasicBlock.Successor = afterIfBasicBlock;
                            conditionBasicBlock.FalseSuccessor = elseBasicBlock;
                        }
                        else
                        {
                            conditionBasicBlock.FalseSuccessor = afterIfBasicBlock;
                        }

                        SetCurrentBasicBlock(afterIfBasicBlock);

                        conditionBasicBlock.TrueSuccessor = ifTrueBasicBlock;
                        
                    }
                    else
                    {
                        base.Visit(node);
                    }
                }
            }

            private void SetCurrentBasicBlock(ControlFlowBasicBlock newCurrentBasicBlock)
            {
                basicBlocks.Add(currentBasicBlock);
                currentBasicBlock = newCurrentBasicBlock;
            }
        }
    }
}
