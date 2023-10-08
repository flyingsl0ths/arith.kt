package org.arith.utils

fun <T> atLeastNValues(n: Int): (List<T>) -> Boolean = { values: List<T> -> values.size == n }

fun <T, U> guard(
    predicate: (T) -> Boolean,
    l: (T) -> U,
    r: (T) -> U
): (T) -> U = {
    if (!predicate(it)) {
        l(it)
    } else {
        r(it)
    }
}

fun <T> atLeastNArgs(
    functionName: String,
    n: Int = 1,
    f: (List<T>) -> T
): (List<T>) -> Either<String, T> =
    guard(atLeastNValues(n), {
        left("$functionName: Expects $n argument(s)")
    }) { right(f(it)) }
