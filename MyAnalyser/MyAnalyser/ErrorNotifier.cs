using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MyAnalyser
{
    public class Error
    {
        public string Text;
        public Location Location;

        public Error(string text, Location location)
        {
            Text = text;
            Location = location;
        }
    }

    public class ErrorNotifier
    {
        public Dictionary<Location,Error> Errors = new Dictionary<Location, Error>();

        public ErrorNotifier()
        {
        }

        public void AddTypeOwerflow(Location location)
        {
            if (Errors.ContainsKey(location)) return;
            Errors.Add(location,new Error("Possible type owerflow.", location));
        }
    }
}