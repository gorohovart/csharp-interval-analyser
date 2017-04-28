using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MyAnalyser.VarStructures
{
    //public class Variable
    //{
    //    public string Type;
    //    public bool IsArray;
    //    public bool IsNullable;

    //    public Variable()
    //    {
    //        Type = type;
    //        IsArray = false;
    //        //IsNullable = false;
    //    }

    //    public Variable GetCopy()
    //    {
    //        var prim = this as Primitive;
    //        if (prim != null)
    //        {
    //            return prim.GetCopy();
    //        }
    //        //var nullable = this as Nullable;
    //        //if (nullable != null)
    //        //{
    //        //    return nullable.GetCopy();
    //        //}

    //        return new Variable(Type);
    //    }

    //    public Variable Concat(Variable otherVar)
    //    {
    //        var prim1 = this as Primitive;
    //        var prim2 = otherVar as Primitive;
    //        if (prim1 != null && prim2 != null)
    //        {
    //            return prim1.Concat(prim2);
    //        }

    //        return this.GetCopy();
    //    }
    //}

    public class Primitive// : Variable
    {
        public bool IsArray;
        public Primitive()// : base()
        {
            IsArray = false;
        }

        public Primitive GetCopy()
        {
            var prim = this as PrimitiveValue;
            if (prim != null)
            {
                return prim.GetCopy();
            }
            var primArr = this as PrimitiveArray;
            if (primArr != null)
            {
                return primArr.GetCopy();
            }
            throw new Exception("Error while type casting");
        }

        public Primitive Concat(Primitive otherVar)
        {
            var val1 = this as PrimitiveValue;
            var val2 = otherVar as PrimitiveValue;
            if (val1 != null && val2 != null)
            {
                return val1.Concat(val2);
            }

            var arr1 = this as PrimitiveArray;
            var arr2 = otherVar as PrimitiveArray;
            if (arr1 != null && arr2 != null)
            {
                return arr1.Concat(arr2);
            }
            throw new Exception("Error while type casting");
        }

        public override bool Equals(object obj)
        {
            var otherPrim = obj as Primitive;
            if (otherPrim != null)
            {
                var val1 = this as PrimitiveValue;
                var val2 = otherPrim as PrimitiveValue;
                if (val1 != null && val2 != null)
                {
                    return val1.Equals(val2);
                }

                var arr1 = this as PrimitiveArray;
                var arr2 = otherPrim as PrimitiveArray;
                if (arr1 != null && arr2 != null)
                {
                    return arr1.Equals(arr2);
                }
                return false;
            }
            return base.Equals(obj);
        }
    }

    //public class Nullable : Variable
    //{
    //    public Nullable() : base()
    //    {
    //        IsNullable = true;
    //    }

    //    public Nullable GetCopy()
    //    {
    //        var nullable = this as NullableValue;
    //        if (nullable != null)
    //        {
    //            return nullable.GetCopy();
    //        }
    //        var nullableArr = this as NullableArray;
    //        if (nullableArr != null)
    //        {
    //            return nullableArr.GetCopy();
    //        }
    //        throw new Exception("Error while type casting");
    //    }
    //}


    public class PrimitiveValue : Primitive
    {
        public HashSet<Interval> Intervals;

        public int GetLow()
        {
            return Intervals.Min(i => i.Low);
        }

        public int GetHigh()
        {
            return Intervals.Max(i => i.High);
        }

        public PrimitiveValue() : base()
        {
            Intervals = new HashSet<Interval> {Interval.Get(0, 0)};
        }

        //public PrimitiveValue( List<Interval> values) : base()
        //{
        //    Intervals = values;
        //}

        public PrimitiveValue( int low, int high) : base()
        {
            Intervals = new HashSet<Interval> {Interval.Get(low, high)};
        }
        public PrimitiveValue(Interval interval) : base()
        {
            Intervals = new HashSet<Interval> { interval };
        }

        public PrimitiveValue GetCopy()
        {
            var x = new PrimitiveValue();
            foreach (var elem in Intervals)
            {
                x.Intervals.Add(elem);
            }
            return x;
        }

        public PrimitiveValue Concat(PrimitiveValue otherVar)
        {
            var x = this.GetCopy();
            x.Intervals.UnionWith(otherVar.Intervals);

            return x;
        }

        public override bool Equals(object obj)
        {
            var otherPrimVal = obj as PrimitiveValue;
            if (otherPrimVal != null)
            {
                var otherIntervals = otherPrimVal.Intervals;
                if (Intervals.Count != otherIntervals.Count) return false;
                foreach (var interval in Intervals)
                {
                    if (!otherIntervals.Contains(interval)) return false;
                }
                return true;
            }
            return base.Equals(obj);
        }
    }

    public class PrimitiveArray : Primitive
    {
        public Primitive[] Elements;
        public Interval Length;
        public PrimitiveArray() : base()
        {
            IsArray = true;
        }

        public PrimitiveArray( int length) : base()
        {
            IsArray = true;
            Elements = new Primitive[length];
        }

        public PrimitiveArray GetCopy()
        {
            var x = new PrimitiveArray();
            x.Elements = new Primitive[Elements.Length];
            for (var i = 0; i < Elements.Length; i++)
            {
                x.Elements[i] = Elements[i].GetCopy();
            }
            return x;
        }

        public PrimitiveArray Concat(PrimitiveArray otherVar)
        {
            var x = GetCopy();
            for (var i = 0; i < Elements.Length; i++)
            {
                x.Elements[i] = Elements[i].Concat(otherVar.Elements[i]);
            }
            return x;
        }

        public override bool Equals(object obj)
        {
            var otherPrimArr = obj as PrimitiveArray;
            if (otherPrimArr != null)
            {
                var otherElems = otherPrimArr.Elements;
                if (Elements.Length != otherElems.Length) return false;
                for (var i = 0; i < Elements.Length; i++)
                {
                    if (!otherElems[i].Equals(Elements[i])) return false;
                }
                return true;
            }
            return base.Equals(obj);
        }

        }


    //public class NullableValue : Nullable
    //{
    //    public List<NullableVarValue> Values;

    //    public NullableValue() : base()
    //    {
    //    }

    //    public NullableValue( List<NullableVarValue> values) : base()
    //    {
    //        Values = values;
    //    }

    //    public NullableValue GetCopy()
    //    {
    //        var x = new NullableValue(Type);
    //        foreach (var elem in Values)
    //        {
    //            x.Values.Add(elem);
    //        }
    //        return x;
    //    }
    //}

    //public class NullableArray : Nullable
    //{
    //    public Nullable[] Elements;

    //    public NullableArray() : base()
    //    {
    //        IsArray = true;
    //    }
    //    public NullableArray( int length) : base()
    //    {
    //        IsArray = true;
    //        Elements = new Nullable[length];
    //    }

    //    public NullableArray GetCopy()
    //    {
    //        var x = new NullableArray(Type);
    //        x.Elements = new Nullable[Elements.Length];
    //        for (var i = 0; i < Elements.Length; i++)
    //        {
    //            x.Elements[i] = Elements[i].GetCopy();
    //        }
    //        return x;
    //    }
    //    }

}