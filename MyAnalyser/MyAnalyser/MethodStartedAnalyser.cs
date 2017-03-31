using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using CSharpAnalyzers;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MyAnalyser
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MethodStartedAnalyser : DiagnosticAnalyzer
    {
        #region Descriptor fields

        internal static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.MethodStartedAnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        internal static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.MethodStartedAnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        internal static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.MethodStartedAnalyzerDescription), Resources.ResourceManager, typeof(Resources));

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor("IntervalAnalysys", Title, MessageFormat, "SampleStatefulAnalyzers", DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        #endregion

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterCodeBlockStartAction<SyntaxKind>(startCodeBlockContext =>
            {
                // We only care about method bodies.
                if (startCodeBlockContext.OwningSymbol.Kind != SymbolKind.Method)
                {
                    return;
                }

                // We only care about methods with parameters.
                var method = (IMethodSymbol)startCodeBlockContext.OwningSymbol;
                //if (method.Parameters.IsEmpty)
                //{
                //    return;
                //}
                var rootOfBlock = startCodeBlockContext.CodeBlock;
                try
                {
                    var x = new MethodAnalyser(method, rootOfBlock, startCodeBlockContext.SemanticModel, Rule);
                    startCodeBlockContext.RegisterCodeBlockEndAction(x.ShowVariablesList);
                }
                catch (Exception e)
                {
                    
                }


                // Initialize local mutable state in the start action.
                //var analyzer = new UnusedParametersAnalyzer(method);

                //// Register an intermediate non-end action that accesses and modifies the state.
                //startCodeBlockContext.RegisterSyntaxNodeAction(analyzer.AnalyzeSyntaxNode, SyntaxKind.IdentifierName);

                //// Register an end action to report diagnostics based on the final state.
                
            });
        }

        private class UnusedParametersAnalyzer
        {
            #region Per-CodeBlock mutable state

            private readonly HashSet<IParameterSymbol> _unusedParameters;
            private readonly HashSet<string> _unusedParameterNames;

            #endregion

            #region State intialization

            public UnusedParametersAnalyzer(IMethodSymbol method)
            {
                // Initialization: Assume all parameters are unused.
                var parameters = method.Parameters.Where(p => !p.IsImplicitlyDeclared && p.Locations.Length > 0);
                _unusedParameters = new HashSet<IParameterSymbol>(parameters);
                _unusedParameterNames = new HashSet<string>(parameters.Select(p => p.Name));
            }

            #endregion

            #region Intermediate actions

            public void AnalyzeSyntaxNode(SyntaxNodeAnalysisContext context)
            {
                // Check if we have any pending unreferenced parameters.
                if (_unusedParameters.Count == 0)
                {
                    return;
                }

                // Syntactic check to avoid invoking GetSymbolInfo for every identifier.
                var identifier = (IdentifierNameSyntax)context.Node;
                if (!_unusedParameterNames.Contains(identifier.Identifier.ValueText))
                {
                    return;
                }

                // Mark parameter as used.
                var parameter = context.SemanticModel.GetSymbolInfo(identifier, context.CancellationToken).Symbol as IParameterSymbol;
                if (parameter != null && _unusedParameters.Contains(parameter))
                {
                    _unusedParameters.Remove(parameter);
                    _unusedParameterNames.Remove(parameter.Name);
                }
            }

            #endregion

            #region End action

            public void CodeBlockEndAction(CodeBlockAnalysisContext context)
            {
                // Report diagnostics for unused parameters.
                foreach (var parameter in _unusedParameters)
                {
                    var diagnostic = Diagnostic.Create(Rule, parameter.Locations[0], parameter.Name, parameter.ContainingSymbol.Name);
                    context.ReportDiagnostic(diagnostic);
                }
            }

            #endregion
        }
    }
}
