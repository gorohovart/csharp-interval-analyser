using System.Collections.Generic;

namespace MyAnalyser.VarStructures
{
    public class Variables
    {
        public Dictionary<string, Primitive> Values;

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

        //public Variable GetValues(string name)
        //{
        //    return Values[name];
        //}

        //public bool ContainsVariable(string name)
        //{
        //    return Values.ContainsKey(name);
        //}

        //public bool TryGetValue(string name, out List<Interval<int>> outValue)
        //{
        //    return Values.TryGetValue(name, out outValue);
        //}

        //public void Add(string name, List<Interval<int>> list)
        //{
        //    Values.Add(name, list);
        //}
    }
}