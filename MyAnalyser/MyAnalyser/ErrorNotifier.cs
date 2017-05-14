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

        public override bool Equals(object obj)
        {
            var other = obj as Error;
            if (other != null)
                return Text == other.Text;
            else
                return base.Equals(obj);
        }

        public override int GetHashCode()
        {
            return Text.GetHashCode();
        }
    }

    public class ErrorNotifier
    {
        public Dictionary<Location,HashSet<Error>> Errors = new Dictionary<Location, HashSet<Error>>();

        public ErrorNotifier()
        {
        }

        public void AddError(Location location, string text)
        {
            HashSet<Error> list;
            if (Errors.TryGetValue(location, out list))
                list.Add(new Error(text, location));
            else
            {
                Errors.Add(location, new HashSet<Error> { new Error(text, location) });
            }
        }

        public void AddDevisionByZero(Location location)
        {
            AddError(location, "Possible devision by zero.");
        }

        public void AddTypeOwerflow(Location location)
        {
            AddError(location, "Possible type owerflow.");
        }

        public void AddOutOfArrayBounds(Location location)
        {
            AddError(location, "Possible out of bounds.");
        }
        public void AddAlwaysTrue(Location location)
        {
            AddError(location, "Condition is always true.");
        }
        public void AddAlwaysFalse(Location location)
        {
            AddError(location, "Condition is always false.");
        }
    }
}