using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MyAnalyser.Variables
{
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
