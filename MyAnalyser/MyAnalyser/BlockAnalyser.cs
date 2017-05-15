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
        private Variables lastInVariables;
        private Variables vars;
        private readonly ControlFlowBasicBlock block;
        private readonly SemanticModel semanticModel;
        private readonly ErrorNotifier errorNotifier;
        private static long minValue = Int32.MinValue;
        private static long maxValue = Int32.MaxValue;

        private static Dictionary<ControlFlowBasicBlock, BlockAnalyser> instanceHolder = new Dictionary<ControlFlowBasicBlock, BlockAnalyser>();

        private BlockAnalyser(ControlFlowBasicBlock block,
            SemanticModel semanticModel, ErrorNotifier errorNotifier)
        {
            this.semanticModel = semanticModel;
            this.block = block;
            this.errorNotifier = errorNotifier;
        }

        public static BlockAnalyser GetInstance(ControlFlowBasicBlock block, SemanticModel semanticModel,
            ErrorNotifier errorNotifier)
        {
            BlockAnalyser current;
            if (instanceHolder.TryGetValue(block, out current))
            {
                return current;
            }
            current = new BlockAnalyser(block, semanticModel, errorNotifier);
            instanceHolder.Add(block,current);
            return current;
        }

        public static BlockAnalyser GetInstanceForExpr(Variables variables, SemanticModel semanticModel, ErrorNotifier notifier)
        {
            var ba = new BlockAnalyser(new ControlFlowBasicBlock(), semanticModel, notifier);
            ba.vars = variables;
            return ba;
        }

        public bool TryToRun(Func<Variables, Variables, Variables> Widening, Variables newVars, out Variables outVariables)
        {
            var newVarsWided = block.NeedWidening ? Widening(lastInVariables, newVars) : newVars; 
            if (newVarsWided.Equals(lastInVariables))
            {
                outVariables = null;
                return false;
            }
            this.lastInVariables = newVarsWided;
            this.vars = new Variables(newVarsWided);
            
            foreach (var statement in block.Statements)
            {
                if (statement.IsKind(SyntaxKind.LocalDeclarationStatement))
                {
                    var localDecl = (LocalDeclarationStatementSyntax)statement;
                    var variableDecl = localDecl.Declaration;
                    VariableDeclrnStatement(variableDecl);
                }
                else if (statement.IsKind(SyntaxKind.VariableDeclaration))
                {
                    var variableDecl = (VariableDeclarationSyntax)statement;
                    VariableDeclrnStatement(variableDecl);
                }
                else if (statement.IsKind(SyntaxKind.ReturnStatement))
                    ReturnStatement((ReturnStatementSyntax)statement);
                else if (statement.IsKind(SyntaxKind.ExpressionStatement))
                {
                    var exprStmt = (ExpressionStatementSyntax)statement;
                    var expr = exprStmt.Expression;
                    StatementExpression(expr);
                }
                else if (statement is ExpressionSyntax)
                {
                    StatementExpression((ExpressionSyntax)statement);
                }
                //else
                //    throw new Exception("todo unsupported type of expression");
            }

            outVariables = vars;
            return true;
        }

        private static bool TryGetType(Type type, string typeName, out string outType, out bool isArray)
        {
            string[] alowedTypes = { "int", "uint",
                                     "short", "ushort",
                                     "long", "ulong",
                                     "sbyte", "bool"};
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

        private long makeOp(Func<long, long, long> op, Func<long, long, long> getVal, long left, long right, Location location)
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
            catch (System.DivideByZeroException e)
            {
                return 0;
            }
        }



        private long[] Op(Func<long, long, long> op, Func<long, long, long> getVal, Interval left, Interval right, Location location)
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

        private Interval ToBool(Interval interval)
        {
            return Interval.Get(interval.Low <= 0 ? 0 : 1, interval.High >= 1 ? 1 : 0);
        }

        private long[] BoolOp(Func<bool, bool, bool> op, Interval leftInterval, Interval rightInterval)
        {
            var left = ToBool(leftInterval);
            var right = ToBool(rightInterval); 
            var ll = left.Low != 0;
            var lh = left.High != 0;
            var rl = right.Low != 0;
            var rh = right.High != 0;
            var x = new[]
            {
                op(ll, rl) ? 1L: 0L,
                op(lh, rl) ? 1L: 0L,
                op(ll, rh) ? 1L: 0L,
                op(lh, rh) ? 1L: 0L
            };
            return x;
        }
        
        private Interval Intersect(Interval left, Interval right)
        {
            var x11 = left.Low;
            var x12 = left.High;
            var x21 = right.Low;
            var x22 = right.High;

            if (Math.Min(x11,x12) < Math.Min(x21, x22))
            { 
                if (Math.Max(x11,x12) < Math.Min(x21,x22))
                {
                    return null;
                }
                else if (Math.Max(x11,x12) == Math.Min(x21,x22))
                    return Interval.Get(Math.Max(x11,x12), Math.Max(x11, x12));
                else if (Math.Max(x11, x12) > Math.Max(x21, x22))
                    return Interval.Get(x21, x22);
                else return Interval.Get(Math.Min(x21, x22), Math.Max(x11, x12));
            }
            else if (Math.Min(x11, x12) == Math.Min(x21, x22))
            {
                if (Math.Max(x11, x12) >= Math.Max(x21, x22))
                    return Interval.Get(x11, x12);
                else return Interval.Get(Math.Min(x11, x12), Math.Max(x21, x22));
            }
            else return Intersect(Interval.Get(x21, x22), Interval.Get(x11, x12));
        }



        private PrimitiveValue BinaryOp(PrimitiveValue left, PrimitiveValue right, string op, Location location)
        {
            var result = new PrimitiveValue(false);
            var leftInterval = Interval.Get(left.Intervals.Min(x => x.Low), left.Intervals.Max(x => x.High));
            var rightInterval = Interval.Get(right.Intervals.Min(x => x.Low), right.Intervals.Max(x => x.High));
            long[] G = new long[4];
            bool flag = false;
            long minG, maxG;
            bool needToCollect = true;
            if (op == "+" || op == "+=" || op == "++")
            {
                G = Op((x, y) => checked(x + y), (x, y) => x > 0 ? maxValue : minValue, leftInterval, rightInterval, location);
            }
            else if (op == "-" || op == "-=" || op == "--")
            {
                G = Op((x, y) => checked(x - y), (x, y) => x > 0 ? minValue : maxValue, leftInterval, rightInterval, location);
            }
            else if (op == "/" || op == "/=")
            {
                //throw new Exception("unsupported division");
                if (rightInterval.Low <= 0 && 0 <= rightInterval.High)
                    errorNotifier.AddDevisionByZero(location);
                G = Op((x, y) => x / y, (x, y) => x > 0 ? minValue : maxValue, leftInterval, rightInterval, location);
            }
            else if (op == "*" || op == "*=")
            {
                G = Op((x, y) => checked(x * y), (x, y) => x > 0 ? (y > 0? maxValue : minValue): (y > 0 ? minValue : maxValue), leftInterval, rightInterval, location);
            }
            else if (op == "||" || op == "|=")
            {
                flag = true;
                G = BoolOp((x, y) => checked(x || y), leftInterval, rightInterval);
            }
            else if (op == "&&" || op == "&=")
            {
                flag = true;
                G = BoolOp((x, y) => checked(x && y), leftInterval, rightInterval);
            }
            else if (op == "==")
            {
                // true if exist intersection
                // false if all - intersection = empty
                flag = true;
                var intersection = Intersect(leftInterval, rightInterval);
                if (intersection == null)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 0;
                }
                else if (leftInterval.Low == leftInterval.High && rightInterval.Low == rightInterval.High)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 1;
                }
                else
                {
                    G[0] = 1;
                    G[1] = 0;
                }
            }
            else if (op == "!=")
            {
                // false if exist intersection
                // true if all - intersection = empty
                flag = true;
                var intersection = Intersect(leftInterval, rightInterval);
                if (intersection == null)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 1;
                }
                else if (leftInterval.Low == leftInterval.High && rightInterval.Low == rightInterval.High)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 0;
                }
                else
                {
                    G[0] = 1;
                    G[1] = 0;
                }
            }
            else if (op == "<=")
            {
                needToCollect = false;
                var res1 = BinaryOp(left, right, "<", location);
                var res2 = BinaryOp(left, right, "==", location);
                result = BinaryOp(res1, res2, "||", location);

            }
            else if (op == ">=")
            {
                needToCollect = false;
                var res1 = BinaryOp(left, right, ">", location);
                var res2 = BinaryOp(left, right, "==", location);
                result = BinaryOp(res1, res2, "||", location);
            }
            else if (op == ">")
            {
                flag = true;
                var a = leftInterval.Low;
                var b = leftInterval.High;
                var c = rightInterval.Low;
                var d = rightInterval.High;
                if (a > c && a > d && b > c && b > d)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 1;
                }
                else if (a < c && a < d && b < c && b < d)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 0;
                }
                else
                {
                    G[0] = 1;
                    G[1] = 0;
                }
            }
            else if (op == "<")
            {
                flag = true;
                var a = leftInterval.Low;
                var b = leftInterval.High;
                var c = rightInterval.Low;
                var d = rightInterval.High;
                if (a > c && a > d && b > c && b > d)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 0;
                }
                else if (a < c && a < d && b < c && b < d)
                {
                    for (var qwe = 0; qwe < 4; qwe++)
                        G[qwe] = 1;
                }
                else
                {
                    G[0] = 1;
                    G[1] = 0;
                }
            }
            else if (op == "is")
            {
                needToCollect = false;
                result.Intervals.Add(Interval.Get(0, 1));
            }
            else
            {
                needToCollect = false;
                result.Intervals.Add(Interval.Get(minValue, maxValue));
                //throw new Exception("unsupported binary operator");
            }

            if (flag)
            {
                if (G.Contains(1) && G.Contains(0))
                {
                    result.Intervals.Add(Interval.Get(0, 1));
                }
                else if (G.Contains(1))
                {
                    result.Intervals.Add(Interval.Get(1, 1));
                }
                else
                {
                    result.Intervals.Add(Interval.Get(0, 0));
                }
            }
            else if (needToCollect)
            {
                minG = G.Min();
                maxG = G.Max();

                result.Intervals.Add(Interval.Get(Math.Max(minG, minValue), Math.Min(maxG, maxValue)));
            }
            return result;
        }

        //private Primitive ConditionalExpression(ExpressionSyntax expr)
        //{
        //    var x = (10 == 4) || (11 == 1) ? 1 : 2;
        //    Primitive result;
        //    var
        //}

        public Primitive Expression(ExpressionSyntax expr)
        {
            var condExpr = expr as ConditionalExpressionSyntax;
            var binExpr = expr as BinaryExpressionSyntax;
            var invocationExpression = expr as InvocationExpressionSyntax;
            var literal = expr as LiteralExpressionSyntax;
            var identifierName = expr as IdentifierNameSyntax;
            var implictArrayCreationExpression = expr as ImplicitArrayCreationExpressionSyntax;
            var arrayCreationExpression = expr as ArrayCreationExpressionSyntax;
            var parExpr = expr as ParenthesizedExpressionSyntax;
            var memberAccessExpr = expr as MemberAccessExpressionSyntax;
            var postUnaryExpr = expr as PostfixUnaryExpressionSyntax;
            var preUnaryExpr = expr as PrefixUnaryExpressionSyntax;
            //var objCreationExpr = expr as ObjectCreationExpressionSyntax;
            Primitive result;
            if (condExpr != null)
            {
                result = Expression(condExpr.Condition);
            }
            else if (binExpr != null)
            {
                var left = (PrimitiveValue)Expression(binExpr.Left);
                var right = (PrimitiveValue)Expression(binExpr.Right);
                if (left == null || right == null)
                    result = null;
                else
                {
                    var operation = binExpr.OperatorToken;
                    result = BinaryOp(left, right, operation.Text, operation.GetLocation());
                }
            }
            else if (invocationExpression != null)
            {
                result = new PrimitiveValue(minValue, maxValue);
            }
            else if (literal != null)
            {
                long value;
                try
                {
                    if (literal.Token.Text == "true" || literal.Token.Text == "false")
                        value = Boolean.Parse(literal.Token.Text) ? 1 : 0;
                    else if (literal.Token.Text == "null")
                    {
                        value = 0;
                    }
                    else if (literal.Token.Text.Contains("\"")||literal.Token.Text.Contains("'"))
                    {
                        value = 0;
                    }
                    else
                        try
                        {
                            value =
                                Int64.Parse(
                                    literal.Token.Text.Replace("L", "")
                                        .Replace("l", "")
                                        .Replace("U", "")
                                        .Replace("u", ""));
                        }
                        catch
                        {
                            value =
                                Int32.Parse(
                                    literal.Token.Text.Replace("L", "")
                                        .Replace("l", "")
                                        .Replace("U", "")
                                        .Replace("u", ""));
                        }
                }
                catch (OverflowException e)
                {
                    value = maxValue;
                }

                result = new PrimitiveValue(value, value);
            }
            else if (identifierName != null)
            {
                var identName = identifierName.Identifier.Text;
                if (vars.Values.ContainsKey(identName))
                    result = vars.Values[identName];
                else
                    result = new PrimitiveValue(minValue, maxValue);
            }
            else if (implictArrayCreationExpression != null)
            {
                if (implictArrayCreationExpression.Initializer != null)
                {
                    var lengthOfArray = implictArrayCreationExpression.Initializer.Expressions.Count;
                    var elemsOfArray = implictArrayCreationExpression.Initializer.Expressions;

                    result = new PrimitiveArray(lengthOfArray);
                    for (var i = 0; i < lengthOfArray; i++)
                    {
                        var value = VariableInitializer(elemsOfArray[i]);
                        ((PrimitiveArray)result).Elements[i] = value;
                    }
                }
                else
                {
                    throw new Exception("Impossible");
                }
            }
            else if (parExpr != null)
            {
                result = Expression(parExpr.Expression);
            }
            else if (memberAccessExpr != null)
            {
                if (memberAccessExpr.Name.ToString() == "this")
                {
                    result = Expression(memberAccessExpr.Expression);
                }
                else
                {
                    result = new PrimitiveValue(minValue, maxValue);
                }
            }
            else if (postUnaryExpr != null || preUnaryExpr != null)
            {
                var x = postUnaryExpr == null ? preUnaryExpr.Operand : postUnaryExpr.Operand;
                var operatorToken = postUnaryExpr == null ? preUnaryExpr.OperatorToken : postUnaryExpr.OperatorToken;
                var one = new PrimitiveValue(1, 1);
                //postUnaryExpr.Operand;
                if (x.IsKind(SyntaxKind.IdentifierName)&&(operatorToken.Text != "!")&&(operatorToken.Text != "-"))
                {
                    var varName = ((IdentifierNameSyntax)x).Identifier.Text;
                    result = AssignmentOperatorHandler(vars.Values[varName], one, operatorToken);
                    vars.Values.Remove(varName);
                    vars.Values.Add(varName, result);
                }
                else if (x.IsKind(SyntaxKind.ElementAccessExpression))
                {
                    throw new Exception("Unsupported ElementAccessExpression expression");
                    //var elementAccess = (ElementAccessExpressionSyntax)x;
                    //var arrayName = ((IdentifierNameSyntax)elementAccess.Expression).Identifier.Text;
                    //var args = elementAccess.ArgumentList.Arguments;
                    //var accessedArray = ErrorsCheck(arrayName, args);
                    //res = AssignmentOperatorHandler(accessedArray.Elements[0], one, operatorToken);
                    //accessedArray.Elements[0] = res;
                }
                else if (operatorToken.Text == "-")
                {
                    if (x is LiteralExpressionSyntax)
                    {
                        var value =
                            Int64.Parse(
                                (x as LiteralExpressionSyntax).Token.Text.Replace("L", "")
                                .Replace("l", "")
                                .Replace("U", "")
                                .Replace("u", ""));
                        result = new PrimitiveValue(-1 * value, -1 * value);
                    }
                    else
                    {
                        var res = Expression(x);
                        result = BinaryOp(res as PrimitiveValue, new PrimitiveValue(-1, -1), "*", x.GetLocation());
                    }
                    //throw new Exception("Unsupported postDecrement expression");
                }
                else if (operatorToken.Text == "!")
                {
                    var res = Expression(x);
                    if (res == null)
                        result = null;
                    else
                    {
                        var nRes = res as PrimitiveValue;
                        var nIntervals = new HashSet<Interval>();
                        foreach (var interval in nRes.Intervals)
                        {
                            if (interval.Low == interval.High)
                                nIntervals.Add(Interval.Get(interval.Low == 1 ? 0 : 1, interval.High == 1 ? 0 : 1));
                            else
                                nIntervals.Add(interval);
                        }
                        result = new PrimitiveValue(nIntervals);
                    }
                }
                else
                    throw new Exception("Unsupported postDecrement expression");
            }
            //else if (objCreationExpr != null)
            //{
            //    result = new PrimitiveValue(minValue, maxValue);
            //}
            /*
            else if (arrayCreationExpression != null)
            {
                //result = Expression(arrayCreationExpression.Initializer);
                var lengthOfArray = arrayCreationExpression.Type.RankSpecifiers[0]; /// rankSpecifiers : [...], [...], [...]
                var elemsOfArray = implictArrayCreationExpression.Initializer.Expressions;

                var posibleLengths = lengthOfArray.Sizes; /// sizes: [..., ..., ...]
                result = new PrimitiveArray(lengthOfArray);
                var current = 
                for (var i = 0; i < lengthOfArray; i++)
                {
                    var value = VariableInitializer(elemsOfArray[i]);
                    ((PrimitiveArray)result).Elements[i] = value;
                }
            }*/
            else
                result = new PrimitiveValue(minValue, maxValue);
                //throw new Exception("todo: unsupported expression type");
            return result;
        }

        //private void UnaryExpression(ExpressionSyntax expr, Variables vars)

        private PrimitiveArray ErrorsCheck(string arrayName, SeparatedSyntaxList<ArgumentSyntax> args)
        {
            Primitive var;
            vars.Values.TryGetValue(arrayName, out var);

            PrimitiveArray checkedArray = (PrimitiveArray)var;
            if (checkedArray == null) throw new Exception("Got not an array from array variable");
            for (var i = 0; i < args.Count; i++)
            {
                var length = checkedArray.Elements.Length;
                var exprRes = Expression(args[i].Expression);
                if (exprRes == null) throw new Exception();
                var accessValues = (PrimitiveValue)exprRes;
                foreach (var interval in accessValues.Intervals)
                {
                    var isInBounds = (interval.Low >= 0) && (interval.Low <= length) && (interval.High >= 0) && (interval.High <= length);
                    if (!isInBounds)
                    {
                        errorNotifier.AddOutOfArrayBounds(args[i].GetLocation());
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
                return BinaryOp((PrimitiveValue)oldValue, (PrimitiveValue)newValue, op.Text, op.GetLocation());
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
                Primitive exprValue;
                if (assignmentExpr.Left.IsKind(SyntaxKind.IdentifierName))
                {
                    var varName = ((IdentifierNameSyntax)assignmentExpr.Left).Identifier.Text;
                    if (vars.Values.ContainsKey(varName))
                    {
                        exprValue = Expression(assignmentExpr.Right);
                        if (exprValue != null)
                        {
                            var result = AssignmentOperatorHandler(vars.Values[varName], exprValue, operatorToken);
                            vars.Values.Remove(varName);
                            vars.Values.Add(varName, result);
                        }
                    }
                }
                else if (assignmentExpr.Left.IsKind(SyntaxKind.ElementAccessExpression))
                {
                    var elementAccess = (ElementAccessExpressionSyntax)assignmentExpr.Left;
                    if (elementAccess.Expression is IdentifierNameSyntax)
                    {
                        var arrayName = ((IdentifierNameSyntax) elementAccess.Expression).Identifier.Text;
                        if (vars.Values.ContainsKey(arrayName))
                        {
                            exprValue = Expression(assignmentExpr.Right);
                            if (exprValue != null)
                            {
                                var args = elementAccess.ArgumentList.Arguments;
                                var accessedArray = ErrorsCheck(arrayName, args);
                                var result = AssignmentOperatorHandler(accessedArray.Elements[0], exprValue,
                                    operatorToken);
                                accessedArray.Elements[0] = result;
                            }
                        }
                    }
                }
            }
            else if (invocationExpr != null)
            {
                // todo: check out params
            }
            else if (postUnaryExpr != null || preUnaryExpr != null)
            {
                var x = postUnaryExpr == null ? preUnaryExpr.Operand : postUnaryExpr.Operand;
                var operatorToken = postUnaryExpr == null ? preUnaryExpr.OperatorToken : postUnaryExpr.OperatorToken;
                var one = new PrimitiveValue(1,1);
                //postUnaryExpr.Operand;
                if (x.IsKind(SyntaxKind.IdentifierName))
                {
                    var varName = ((IdentifierNameSyntax)x).Identifier.Text;
                    if (vars.Values.ContainsKey(varName))
                    {

                        var result = AssignmentOperatorHandler(vars.Values[varName], one, operatorToken);
                        vars.Values.Remove(varName);
                        vars.Values.Add(varName, result);
                    }
                }
                else if (x.IsKind(SyntaxKind.ElementAccessExpression))
                {
                    var elementAccess = (ElementAccessExpressionSyntax)x;
                    var arrayName = ((IdentifierNameSyntax)elementAccess.Expression).Identifier.Text;
                    var args = elementAccess.ArgumentList.Arguments;
                    var accessedArray = ErrorsCheck(arrayName, args);
                    var result = AssignmentOperatorHandler(accessedArray.Elements[0], one, operatorToken);
                    accessedArray.Elements[0] = result;
                }
                //else
                    //throw new Exception("Unsupported postDecrement expression");
            }
            else if (objCreationExpr != null)
            {
                //throw new Exception("todo unsupported objCreation expression");
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

                newVar = new PrimitiveArray(lengthOfArray);
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

        private void VariableDeclrnStatement(VariableDeclarationSyntax variableDecl)
        {
            //var variableDecl = declaration.Declaration;
            //var modifiers = declaration.Modifiers;
            var typeOfVars = semanticModel.GetSymbolInfo(variableDecl.Type);
            var nameOfType = typeOfVars.Symbol.ToString();

            //VariablesList.Add(new Tuple<string, Location>(nameOfType, variableDecl.GetLocation()));
            //printfn "Type: %s" nameOfType
            //if (nameOfType != "int" && nameOfType != "Int32") break;
            //bool isConst = modifiers.Select(x => x.Text).Contains("const");
            string outName;
            bool isArray;
            if (!TryGetType(typeOfVars.GetType(), nameOfType, out outName, out isArray)) return;

            foreach (var variableDeclarator in variableDecl.Variables)
            {
                var varName = variableDeclarator.Identifier.Text;
                if (vars.Values.ContainsKey(varName))
                    vars.Values.Remove(varName);// new Exception("Declaration of already declared variable");

                Primitive newVar;
                if (variableDeclarator.Initializer != null)
                {
                    newVar = VariableInitializer(variableDeclarator.Initializer.Value);
                }
                else if (isArray)
                {
                    newVar = new PrimitiveArray();
                }
                else
                {
                    newVar = new PrimitiveValue();
                }
                vars.Values.Add(varName, newVar);
            }
        }

        private void ReturnStatement(ReturnStatementSyntax expr)
        {
            if (expr.Expression != null)
                Expression(expr.Expression);
            //throw new Exception("todo unsupported RETURN expression");
        }
    }
}