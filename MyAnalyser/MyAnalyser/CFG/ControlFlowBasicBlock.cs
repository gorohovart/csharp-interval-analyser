using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpAnalyzers.ControlFlowGraph
{
    public class ControlFlowBasicBlock
    {
        public ControlFlowBasicBlock TrueSuccessor;
        public ControlFlowBasicBlock FalseSuccessor;
        public ControlFlowBasicBlock Successor;
        public ExpressionSyntax Condition;
        public bool NeedWidening = false;
        public ControlFlowBasicBlock()
        {
            Statements = new List<SyntaxNode>();
            //TrueSuccessor = null;//new List<ControlFlowBasicBlock>();
            //FalseSuccessor = new List<ControlFlowBasicBlock>();
        }
        public ControlFlowBasicBlock(bool needWidening)
        {
            Statements = new List<SyntaxNode>();
            NeedWidening = needWidening;
            //TrueSuccessor = null;//new List<ControlFlowBasicBlock>();
            //FalseSuccessor = new List<ControlFlowBasicBlock>();
        }

        public IList<SyntaxNode> Statements { get; private set; }

        public SyntaxNode Terminator;

        //public IList<ControlFlowBasicBlock> Successors { get; set; }
    }
}
