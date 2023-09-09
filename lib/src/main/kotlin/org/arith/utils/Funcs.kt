package org.arith.utils


fun <T> atLeastNValues(n: Int): (List<T>) -> Boolean = { values: List<T> -> values.size == n }

fun <T> guard(
    cause: Pair<String, String>,
    predicate: (List<T>) -> Boolean,
    f: (List<T>) -> T
): (List<T>) -> Either<String, T> = {
    if (!predicate(it)) {
        left("${cause.first}: ${cause.second}")
    } else {
        right(f(it))
    }

}

fun <T> atLeastNArgs(
    functionName: String,
    n: Int = 1,
    f: (List<T>) -> T
): (List<T>) -> Either<String, T> =
    guard(functionName to "Expects one argument", atLeastNValues(n), f)
