module Intervals

type IntValue = 
    | MaxValue
    | MinValue
    | Value of int

type VarValues = 
    | Interval of IntValue * IntValue

