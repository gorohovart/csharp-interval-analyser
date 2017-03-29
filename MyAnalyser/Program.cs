using System;
using System.Linq;
using Diagnostics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MyAnalyser
{
    class Program
    {
        static void Main(string[] args)
        {
            SyntaxNode node = ParseStatement("a = 0;");

            ControlFlowGraph cfg = ControlFlowGraph.Create(node);
            Console.WriteLine(cfg.ToGraph());
        }

        private static SyntaxNode ParseStatement(String statement)
        {
            SyntaxTree tree = CSharpSyntaxTree.ParseText("namespace A { class A() { A() { " + statement + " } } }");
            
            return tree.GetRoot();
        }
    }
}
