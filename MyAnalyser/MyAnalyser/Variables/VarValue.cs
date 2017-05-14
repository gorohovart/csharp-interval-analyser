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
        public long Value;
        public NumValue(long v)
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
        public long Low { get; }
        public long High { get; }
        public string Type { get; }
        private static Dictionary<long,Dictionary<long,Interval>> instanceHolder = new Dictionary<long, Dictionary<long, Interval>>();

        private Interval(long low, long high)
        {
            Low = low;
            High = high;
        }

        public static Interval Get(long low, long high)
        {
            Dictionary<long, Interval> innerDict;
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
            var newInner = new Dictionary<long, Interval> {{high, newInst}};
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
            return (int)(Low ^ (High >> 32));
        }
    }
}
