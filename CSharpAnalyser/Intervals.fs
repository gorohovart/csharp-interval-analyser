module Intervals

type IntValue = 
    | MaxValue
    | MinValue
    | Value of int

type VarValues = 
    | Noninit 
    | Interval of IntValue * IntValue

