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

    //    public Variable(string type)
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
        public string Type;
        public bool IsArray;
        public Primitive(string type)// : base(type)
        {
            Type = type;
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

    }

    //public class Nullable : Variable
    //{
    //    public Nullable(string type) : base(type)
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
        public List<Interval<int>> Intervals;

        public PrimitiveValue(string type) : base(type)
        {
            Intervals = new List<Interval<int>>{new Interval<int>(0,0)};
        }

        public PrimitiveValue(string type, List<Interval<int>> values) : base(type)
        {
            Intervals = values;
        }

        public PrimitiveValue(string type, int low, int high) : base(type)
        {
            Intervals = new List<Interval<int>>{new Interval<int>(low, high)};
        }

        public PrimitiveValue GetCopy()
        {
            var x = new PrimitiveValue(Type);
            foreach (var elem in Intervals)
            {
                x.Intervals.Add(elem);
            }
            return x;
        }

        public PrimitiveValue Concat(PrimitiveValue otherVar)
        {
            var x = new PrimitiveValue(Type);
            x.Intervals = Intervals.Concat(otherVar.Intervals).Distinct().ToList();
            
            return x;
        }
    }

    public class PrimitiveArray : Primitive
    {
        public Primitive[] Elements;

        public PrimitiveArray(string type) : base(type)
        {
            IsArray = true;
        }

        public PrimitiveArray(string type, int length) : base(type)
        {
            IsArray = true;
            Elements = new Primitive[length];
        }

        public PrimitiveArray GetCopy()
        {
            var x = new PrimitiveArray(Type);
            x.Elements = new Primitive[Elements.Length];
            for (var i = 0; i < Elements.Length; i++)
            {
                x.Elements[i] = Elements[i].GetCopy();
            }
            return x;
        }

        public PrimitiveArray Concat(PrimitiveArray otherVar)
        {
            var x = new PrimitiveArray(Type);
            for (var i = 0; i < Elements.Length; i++)
            {
                x.Elements[i] = Elements[i].Concat(otherVar.Elements[i]);
            }
            return x;
        }
    }


    //public class NullableValue : Nullable
    //{
    //    public List<NullableVarValue> Values;

    //    public NullableValue(string type) : base(type)
    //    {
    //    }

    //    public NullableValue(string type, List<NullableVarValue> values) : base(type)
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

    //    public NullableArray(string type) : base(type)
    //    {
    //        IsArray = true;
    //    }
    //    public NullableArray(string type, int length) : base(type)
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