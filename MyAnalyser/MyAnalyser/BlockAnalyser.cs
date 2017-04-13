using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using CSharpAnalyzers.ControlFlowGraph;
using Microsoft.CSharp.RuntimeBinder;
using MyAnalyser.VarStructures;

namespace MyAnalyser
{
    public class BlockAnalyser
    {
        private Variables vars;
        private ControlFlowBasicBlock block;
        private SemanticModel semanticModel;
        private ErrorNotifier errorNotifier;
        private static int minValue = Int32.MinValue;
        private static int maxValue = Int32.MaxValue;

        public BlockAnalyser(ControlFlowBasicBlock block,
            Variables vars, SemanticModel semanticModel, ErrorNotifier errorNotifier)
        {
            this.vars = new Variables(vars);
            this.semanticModel = semanticModel;
            this.block = block;
            this.errorNotifier = errorNotifier;
        }

        public Variables Run()
        {
            foreach (var statement in block.Statements)
            {
                if (statement.IsKind(SyntaxKind.LocalDeclarationStatement))
                {
                    var variableDecl = (LocalDeclarationStatementSyntax)statement;
                    DeclarationStatement(variableDecl);
                }
                else if (statement.IsKind(SyntaxKind.ExpressionStatement))
                {
                    var exprStmt = (ExpressionStatementSyntax)statement;
                    var expr = exprStmt.Expression;
                    StatementExpression(expr);
                }
                else if (statement.IsKind(SyntaxKind.ReturnStatement))
                    ReturnStatement((ReturnStatementSyntax)statement);
                else
                    throw new Exception("todo unsupported type of expression");
            }

            return vars;
        }

        private static bool TryGetType(Type type, string typeName, out string outType, out bool isArray)
        {
            string[] alowedTypes = { "int" };
            isArray = type.GetTypeInfo().IsArray;
            //var typeInfo = type.GetTypeInfo();
            //if (type.GetTypeInfo().IsPrimitive)
            //{
            if (alowedTypes.Contains(typeName))
            {
                outType = typeName;
                return true;
            }
            outType = "";
            return false;
            //}
            //outType = typeName;
            //return false;
        }

        private int makeOp(Func<int, int, int> op, Func<int, int, int> getVal, int left, int right, Location location)
        {
            try
            {
                var x = op(left, right);
                return x;
            }
            catch (System.OverflowException e)
            {
                errorNotifier.AddTypeOwerflow(location);
                return getVal(left, right);
            }
        }

        private int[] Op(Func<int, int, int> op, Func<int, int, int> getVal, Interval<int> left, Interval<int> right, Location location)
        {
            var x = new[]
            {
                makeOp(op, getVal, left.Low, right.Low, location),
                makeOp(op, getVal, left.High, right.Low, location),
                makeOp(op, getVal, left.Low, right.High, location),
                makeOp(op, getVal, left.High, right.High, location)
            };
            return x;
        }

        private PrimitiveValue BinaryOp(PrimitiveValue left, PrimitiveValue right, SyntaxToken op)
        {
            var location = op.GetLocation();
            var result = new PrimitiveValue("");
            foreach (var leftInterval in left.Intervals)
                foreach (var rightInterval in right.Intervals)
                {
                    
                    int[] G;
                    int minG, maxG;
                    if (op.Text == "+" || op.Text == "+=")
                    {
                        G = Op((x, y) => checked(x + y), (x, y) => x > 0 ? maxValue : minValue, leftInterval, rightInterval, location);
                    }
                    else if (op.Text == "-" || op.Text == "-=")
                    {
                        G = Op((x, y) => checked(x - y), (x, y) => x > 0 ? minValue : maxValue, leftInterval, rightInterval, location);
                    }
                    else if (op.Text == "/" || op.Text == "/=")
                    {
                        throw new Exception("unsupported division");
                        //G = Op((x, y) => x - y, (x, y) => x > 0 ? minValue : maxValue, leftInterval, rightInterval, location);
                    }
                    else if (op.Text == "*" || op.Text == "*=")
                    {
                        G = Op((x, y) => checked(x / y), (x, y) => x > 0 ? (y > 0? maxValue : minValue): (y > 0 ? minValue : maxValue), leftInterval, rightInterval, location);
                    }
                    else
                    {
                        throw new Exception("unsupported binary operator");
                    }
                    minG = G.Min();
                    maxG = G.Max();

                    result.Intervals.Add(new Interval<int>(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
                    
                }
            return result;
        }

        //private Primitive ConditionalExpression(ExpressionSyntax expr)
        //{
        //    var x = (10 == 4) || (11 == 1) ? 1 : 2;
        //    Primitive result;
        //    var
        //}

        private Primitive Expression(ExpressionSyntax expr)
        {
            var condExpr = expr as ConditionalExpressionSyntax;
            var binExpr = expr as BinaryExpressionSyntax;
            var invocationExpression = expr as InvocationExpressionSyntax;
            var literal = expr as LiteralExpressionSyntax;
            var identifierName = expr as IdentifierNameSyntax;
            var initializerExpression = expr as InitializerExpressionSyntax;
            Primitive result;
            if (condExpr != null)
            {
                result = Expression(condExpr.Condition);
            }
            else if (binExpr != null)
            {
                var left = (PrimitiveValue)Expression(binExpr.Left);
                var right = (PrimitiveValue)Expression(binExpr.Right);
                var operation = binExpr.OperatorToken;
                result = BinaryOp(left, right, operation);
            }
            else if (invocationExpression != null)
            {
                result = new PrimitiveValue("") { Intervals = new List<Interval<int>> { new Interval<int>(minValue, maxValue) } };
            }
            else if (literal != null)
            {
                int value;
                try
                {
                    value = Int32.Parse(literal.Token.Text);
                }
                catch (OverflowException e)
                {
                    value = maxValue;
                }
                
                result = new PrimitiveValue("") { Intervals = new List<Interval<int>> { new Interval<int>(value, value) } };
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
                    var value = VariableInitializer(elemsOfArray[i]);
                    ((PrimitiveArray)result).Elements[i] = value;
                }
            }
            else
                throw new Exception("todo: unsupported expression type");
            return result;
        }

        //private void UnaryExpression(ExpressionSyntax expr, Variables vars)

        private PrimitiveArray ErrorsCheck(string arrayName, SeparatedSyntaxList<ArgumentSyntax> args)
        {
            Primitive var;
            vars.Values.TryGetValue(arrayName, out var);

            PrimitiveArray checkedArray = (PrimitiveArray)var;
            for (var i = 0; i < args.Count; i++)
            {
                var length = checkedArray.Elements.Length;
                var accessValues = (PrimitiveValue) Expression(args[i].Expression);
                foreach (var interval in accessValues.Intervals)
                {
                    var isInBounds = (interval.Low >= 0) && (interval.Low <= length) && (interval.High >= 0) && (interval.High <= length);
                    if (!isInBounds)
                    {
                        errorNotifier.AddOutOfArrayBounds(args[i].Expression.GetLocation());
                    }
                }
                if (i+1 < args.Count)
                    checkedArray = (PrimitiveArray)checkedArray.Elements[0];
            }

            return checkedArray;
        }

        private Primitive AssignmentOperatorHandler(Primitive oldValue, Primitive newValue, SyntaxToken op)
        {
            if (op.ToString() != "=")
                return BinaryOp((PrimitiveValue)oldValue, (PrimitiveValue)newValue, op);
            return newValue;
        }

        private void StatementExpression(ExpressionSyntax expr)
        {
            var invocationExpr = expr as InvocationExpressionSyntax;
            var assignmentExpr = expr as AssignmentExpressionSyntax;
            var postUnaryExpr = expr as PostfixUnaryExpressionSyntax;
            var preUnaryExpr = expr as PrefixUnaryExpressionSyntax;
            var objCreationExpr = expr as ObjectCreationExpressionSyntax;
            if (assignmentExpr != null)
            {
                var operatorToken = assignmentExpr.OperatorToken;
                var exprValue = Expression(assignmentExpr.Right);
                if (assignmentExpr.Left.IsKind(SyntaxKind.IdentifierName))
                {
                    var varName = ((IdentifierNameSyntax)assignmentExpr.Left).Identifier.Text;
                    var result = AssignmentOperatorHandler(vars.Values[varName], exprValue, operatorToken);
                    vars.Values.Remove(varName);
                    vars.Values.Add(varName, result);
                }
                else if (assignmentExpr.Left.IsKind(SyntaxKind.ElementAccessExpression))
                {
                    var elementAccess = (ElementAccessExpressionSyntax)assignmentExpr.Left;
                    var arrayName = ((IdentifierNameSyntax)elementAccess.Expression).Identifier.Text;
                    var args = elementAccess.ArgumentList.Arguments;
                    var accessedArray = ErrorsCheck(arrayName, args);
                    var result = AssignmentOperatorHandler(accessedArray.Elements[0], exprValue, operatorToken);
                    accessedArray.Elements[0] = result;
                }
            }
            else if (invocationExpr != null)
            {
                // todo: check out params
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

        private Primitive VariableInitializer(ExpressionSyntax expr)
        {
            Primitive newVar;
            if (expr is InitializerExpressionSyntax)
            {
                var initializerExpression = (InitializerExpressionSyntax)expr;
                var lengthOfArray = initializerExpression.Expressions.Count;
                var elemsOfArray = initializerExpression.Expressions;

                newVar = new PrimitiveArray("", lengthOfArray);
                for (var i = 0; i < lengthOfArray; i++)
                {
                    var value = VariableInitializer(elemsOfArray[i]);
                    ((PrimitiveArray)newVar).Elements[i] = value;
                }
            }
            else
            {
                newVar = Expression(expr);
            }
            return newVar;
        }

        private void DeclarationStatement(LocalDeclarationStatementSyntax declaration)
        {
            var variableDecl = declaration.Declaration;
            var modifiers = declaration.Modifiers;
            var typeOfVars = semanticModel.GetSymbolInfo(variableDecl.Type);
            var nameOfType = typeOfVars.Symbol.ToString();

            //VariablesList.Add(new Tuple<string, Location>(nameOfType, variableDecl.GetLocation()));
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
                    newVar = VariableInitializer(variableDeclarator.Initializer.Value);
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

        private void ReturnStatement(ReturnStatementSyntax expr)
        {
            throw new Exception("todo unsupported RETURN expression");
        }
    }
}