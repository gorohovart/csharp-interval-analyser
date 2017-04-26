using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using System.Threading.Tasks;

namespace MyAnalyser.VarStructures
{
    public enum MinMax
    {
        Min = 0,
        Max = 1
    }

    public class Value
    {
        public Value()
        { }
    }

    public class NumValue : Value
    {
        public int Value;
        public NumValue(int v)
        {
            Value = v;
        }
    }

    public class MinMaxValue : Value
    {
        public MinMax Value;
        public MinMaxValue(MinMax v)
        {
            Value = v;
        }
    }

    public class Interval
    {
        public int Low { get; }
        public int High { get; }

        private static Dictionary<int,Dictionary<int,Interval>> instanceHolder = new Dictionary<int, Dictionary<int, Interval>>();

        private Interval(int low, int high)
        {
            Low = low;
            High = high;
        }

        public static Interval Get(int low, int high)
        {
            Dictionary<int, Interval> innerDict;
            if (instanceHolder.TryGetValue(low, out innerDict))
            {
                Interval instance;
                if (!innerDict.TryGetValue(high, out instance))
                {
                    instance = new Interval(low, high);
                    innerDict.Add(high, instance);
                }
                return instance;
            }
            var newInst = new Interval(low, high);
            var newInner = new Dictionary<int, Interval> {{high, newInst}};
            instanceHolder.Add(low, newInner);
            return newInst;
        }

        public override bool Equals(object obj)
        {
            var otherInterval = obj as Interval;
            if (otherInterval != null)
            {
                return (otherInterval.Low == Low && otherInterval.High == High);
            }
            return base.Equals(obj);
        }

        public override int GetHashCode()
        {
            int hash = 23;
            hash = hash * 31 + Low;
            hash = hash * 31 + High;
            return hash;
        }
    }
}
