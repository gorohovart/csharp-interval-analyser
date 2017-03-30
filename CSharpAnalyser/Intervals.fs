module Intervals

type IntValue = 
    | MaxValue
    | MinValue
    | Value of int

[<Struct>]
type Interval = 
    val Low : int
    val High : int
    val Prob : double
    new(low, high, prob) = 
        { Low = low
          High = high
          Prob = prob }
