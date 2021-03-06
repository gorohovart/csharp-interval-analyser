﻿using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpAnalyzers.ControlFlowGraph
{
    public class ControlFlowGraph
    {
        private ControlFlowGraph(IList<ControlFlowBasicBlock> basicBlocks, ControlFlowBasicBlock firstBlock)
        {
            BasicBlocks = basicBlocks;
            FirstBlock = firstBlock;
            InNodes = new Dictionary<ControlFlowBasicBlock, List<ControlFlowBasicBlock>>();
            foreach (var block in basicBlocks)
            {
                if (block.FalseSuccessor != null)
                {
                    if (InNodes.ContainsKey(block.FalseSuccessor))
                        InNodes[block.FalseSuccessor].Add(block);
                    else
                        InNodes.Add(block.FalseSuccessor, new List<ControlFlowBasicBlock> { block });
                }

                if (block.TrueSuccessor != null)
                {
                    if (InNodes.ContainsKey(block.TrueSuccessor))
                        InNodes[block.TrueSuccessor].Add(block);
                    else
                        InNodes.Add(block.TrueSuccessor, new List<ControlFlowBasicBlock> {block});
                }

                if (block.Successor != null)
                {
                    if (InNodes.ContainsKey(block.Successor))
                        InNodes[block.Successor].Add(block);
                    else
                        InNodes.Add(block.Successor, new List<ControlFlowBasicBlock> {block});
                }
            }
        }

        public ControlFlowBasicBlock FirstBlock;
        public Dictionary<ControlFlowBasicBlock, List<ControlFlowBasicBlock>> InNodes;

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
            const int max = 500 - 100 + 200 - 100;
            int x;
            x = 5 + 4;
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
                //SyntaxKind.VariableDeclaration,
                SyntaxKind.LocalDeclarationStatement, 
                SyntaxKind.ExpressionStatement,
                SyntaxKind.ReturnStatement,
                //SyntaxKind.EqualsExpression,
                //SyntaxKind.NotEqualsExpression,
                //SyntaxKind.IfStatement
            }.ToImmutableHashSet();
            private readonly ImmutableHashSet<SyntaxKind> supportedLoopKinds = new[]
            {
                //SyntaxKind.VariableDeclaration,
                SyntaxKind.WhileStatement,
                //SyntaxKind.ForEachStatement,
                SyntaxKind.ForStatement,
                SyntaxKind.DoStatement
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
                    if (supportedKinds.Contains(node.Kind()))
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
                    else if (supportedLoopKinds.Contains(node.Kind()))
                    {
                        LoopProcesser(node);
                    }
                    else
                    {
                        base.Visit(node);
                    }
                }
            }

            private void LoopProcesser(SyntaxNode node)
            {
                var basicBlock = currentBasicBlock;
                var statementBasicBlock = new ControlFlowBasicBlock(true);
                var afterLoopBasicBlock = new ControlFlowBasicBlock();

                basicBlock.Terminator = node;
                if (node.IsKind(SyntaxKind.WhileStatement))
                {
                    var whileSt = (WhileStatementSyntax) node;
                    var condition = whileSt.Condition;
                    var statement = whileSt.Statement;

                    basicBlock.Condition = condition;
                    basicBlock.TrueSuccessor = statementBasicBlock;
                    basicBlock.FalseSuccessor = afterLoopBasicBlock;

                    SetCurrentBasicBlock(statementBasicBlock);
                    Visit(statement);

                    currentBasicBlock.Condition = condition;
                    currentBasicBlock.TrueSuccessor = statementBasicBlock;
                    currentBasicBlock.FalseSuccessor = afterLoopBasicBlock;
                }
                else if (node.IsKind(SyntaxKind.DoStatement))
                {
                    var doSt = (DoStatementSyntax)node;
                    var condition = doSt.Condition;
                    var statement = doSt.Statement;

                    basicBlock.Successor = statementBasicBlock;

                    SetCurrentBasicBlock(statementBasicBlock);
                    Visit(statement);

                    currentBasicBlock.Condition = condition;
                    currentBasicBlock.TrueSuccessor = statementBasicBlock;
                    currentBasicBlock.FalseSuccessor = afterLoopBasicBlock;
                }
                else if (node.IsKind(SyntaxKind.ForEachStatement))
                {
                    throw new Exception("Foreach loop is unsupported");
                    //var foreachSt = (ForEachStatementSyntax)node;
                    //var collectionExpression = foreachSt.Expression;
                    //var statement = foreachSt.Statement;
                    //// тут логика с тем что надо быть в границах коллекции
                    //basicBlock.Condition = expression;
                    //basicBlock.TrueSuccessor = statementBasicBlock;
                    //basicBlock.FalseSuccessor = afterLoopBasicBlock;

                    //SetCurrentBasicBlock(statementBasicBlock);
                    //Visit(statement);

                    //currentBasicBlock.Condition = condition;
                    //currentBasicBlock.TrueSuccessor = statementBasicBlock;
                    //currentBasicBlock.FalseSuccessor = afterLoopBasicBlock;
                }
                else if(node.IsKind(SyntaxKind.ForStatement))
                {
                    var forSt = (ForStatementSyntax)node;
                    var condition = forSt.Condition;
                    var declaration = forSt.Declaration;
                    var incrementors = forSt.Incrementors;
                    var statement = forSt.Statement;

                    var declarationBlock = new ControlFlowBasicBlock();

                    basicBlock.Successor = declarationBlock;
                    declarationBlock.Statements.Add(declaration);
                    declarationBlock.Condition = condition;
                    declarationBlock.TrueSuccessor = statementBasicBlock;
                    declarationBlock.FalseSuccessor = afterLoopBasicBlock;

                    SetCurrentBasicBlock(statementBasicBlock);
                    Visit(statement);

                    var incrementorsBlock = new ControlFlowBasicBlock();
                    currentBasicBlock.Successor = incrementorsBlock;
                    foreach (var incrementor in incrementors)
                    {
                        incrementorsBlock.Statements.Add(incrementor);
                    }
                    incrementorsBlock.Condition = condition;
                    incrementorsBlock.TrueSuccessor = statementBasicBlock;
                    incrementorsBlock.FalseSuccessor = afterLoopBasicBlock;
                }
                SetCurrentBasicBlock(afterLoopBasicBlock);
            }

            private void SetCurrentBasicBlock(ControlFlowBasicBlock newCurrentBasicBlock)
            {
                basicBlocks.Add(currentBasicBlock);
                currentBasicBlock = newCurrentBasicBlock;
            }
        }
    }
}
