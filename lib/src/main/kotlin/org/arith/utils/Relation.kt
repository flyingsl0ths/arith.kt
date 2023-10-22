package org.arith.utils

typealias Result = Double
typealias Calculation = (List<Result>) -> Result
typealias Arity = UInt

sealed interface Relation

data class Unary<T>(val op: (T) -> T) : Relation {
    operator fun invoke(right: T) = op(right)
}

data class Binary<T>(val op: (T, T) -> T) : Relation {
    operator fun invoke(left: T, right: T) = op(left, right)
}

data class Func(val data: Pair<Calculation, Arity>) : Relation