using System;
using System.IO;
using System.Linq;
using DiagnosticsTools;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Analyser
{
    class Program
    {
        static void Main(string[] args)
        {
            SyntaxNode node = ParseStatement(
                "{\n" +
                "    var x;\n" +
                "    x = 7 + 1;\n" +
                "    x = x + 5;\n" +
                "    var y = 3;\n" +
                "    if (x == 7) { a = 0; b = 0; } c = 0; }");

            ControlFlowGraph cfg = ControlFlowGraph.Create(node);
            

            foreach (var block in cfg.BasicBlocks)
            {
                foreach (var statement in block.Statements)
                {
                    Console.WriteLine(statement.ToString());
                    foreach (var n in statement.ChildNodes())
                    {
                        //Console.WriteLine("--->" + n.ToString());
                    }
                    
                }

            }

            File.WriteAllText("if.dot", cfg.ToGraph());
        }

        private static SyntaxNode ParseStatement(String statement)
        {
            SyntaxTree tree = CSharpSyntaxTree.ParseText("namespace A { class A() { A() { " + statement + " } } }");

            Console.WriteLine(tree.ToString());
            return tree.GetRoot();
        }
    }
}
