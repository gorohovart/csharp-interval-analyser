using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace DiagnosticsTools
{
    public class ControlFlowBasicBlock
    {
        public ControlFlowBasicBlock TrueSuccessor;
        public ControlFlowBasicBlock FalseSuccessor;
        public ControlFlowBasicBlock Successor;
        public ExpressionSyntax Condition;
        public ControlFlowBasicBlock()
        {
            Statements = new List<SyntaxNode>();
            //TrueSuccessor = null;//new List<ControlFlowBasicBlock>();
            //FalseSuccessor = new List<ControlFlowBasicBlock>();
        }

        public IList<SyntaxNode> Statements { get; private set; }

        public SyntaxNode Terminator;

        //public IList<ControlFlowBasicBlock> Successors { get; set; }
    }
}
