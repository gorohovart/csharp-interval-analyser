using System;
using System.Collections.Generic;
using System.Linq;
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

    public struct Interval<T>
    {
        public T Low;
        public T High;

        public Interval(T low, T high)
        {
            Low = low;
            High = high;
        }
    }
}
