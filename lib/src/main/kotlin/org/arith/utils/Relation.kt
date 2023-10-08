package org.arith.utils

sealed class Relation {
    data class Unary<T>(val op: (T) -> T) : Relation() {
        operator fun invoke(right: T): T = op(right)
    }

    data class Binary<T>(val op: (T, T) -> T) : Relation() {
        operator fun invoke(left: T, right: T): T = op(left, right)
    }
}
