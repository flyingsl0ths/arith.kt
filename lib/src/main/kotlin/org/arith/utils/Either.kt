package org.arith.utils

sealed interface Either<out L, out R>

data class Left<out T>(val value: T) : Either<T, Nothing>

 class Right<out T>(val value: T) : Either<Nothing, T>

inline fun <T> left(value: T): Left<T> = Left(value)

inline fun <T> right(value: T): Right<T> = Right(value)

inline fun <L, R, T> Either<L, R>.fold(onLeft: (L) -> T, onRight: (R) -> T): T =
    when (this) {
        is Left -> onLeft(value)
        is Right -> onRight(value)
    }

inline fun <L, R, T> Either<L, R>.flatMap(f: (R) -> Either<L, T>): Either<L, T> =
    fold(onLeft = { this as Left }, onRight = f)

inline fun <L, R, T> Either<L, R>.map(f: (R) -> T): Either<L, T> =
    flatMap { Right(f(it)) }
