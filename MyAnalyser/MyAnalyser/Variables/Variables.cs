using System.Collections.Generic;
using System.Linq;

namespace MyAnalyser.VarStructures
{
    public class Variables
    {
        public Dictionary<string, Primitive> Values = new Dictionary<string, Primitive>();

        public Variables()
        {
            
        }

        public Variables(Variables oldVariables)
        {
            Values = new Dictionary<string, Primitive>();
            if (oldVariables.Values == null) return;
            foreach (var pair in oldVariables.Values)
            {
                Values.Add(pair.Key, pair.Value.GetCopy());
            }
        }

        public override bool Equals(object obj)
        {
            var otherVar = obj as Variables;
            if (otherVar != null)
            {
                var otherValues = otherVar.Values;
                if (Values == otherValues) return true;
                if ((Values == null) || (otherValues == null)) return false;
                if (Values.Count != otherValues.Count) return false;
                
                foreach (var kvp in Values)
                {
                    Primitive value2;
                    if (!otherValues.TryGetValue(kvp.Key, out value2)) return false;

                    if (!kvp.Value.Equals(value2)) return false;
                }
                return true;
            }
            return base.Equals(obj);
        }

        //public Variable GetValues(string name)
        //{
        //    return Values[name];
        //}

        //public bool ContainsVariable(string name)
        //{
        //    return Values.ContainsKey(name);
        //}

        //public bool TryGetValue(string name, out List<Interval> outValue)
        //{
        //    return Values.TryGetValue(name, out outValue);
        //}

        //public void Add(string name, List<Interval> list)
        //{
        //    Values.Add(name, list);
        //}
    }
}