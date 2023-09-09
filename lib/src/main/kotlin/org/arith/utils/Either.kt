package org.arith.utils

sealed class Either<out L, out R> {
    data class Left<out T>(val value: T) : Either<T, Nothing>()

    data class Right<out T>(val value: T) : Either<Nothing, T>()

    override fun equals(other: Any?): Boolean {
        if (this === other) return true

        if (javaClass != other?.javaClass) return false

        return if (this is Left<*> && other is Left<*>) {
            this.value == other.value
        } else if (this is Right<*> && other is Right<*>) {
            this.value == other.value
        } else {
            true
        }


    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }

}

inline fun <T> left(value: T): Either.Left<T> = Either.Left(value)

inline fun <T> right(value: T): Either.Right<T> = Either.Right(value)

inline fun <L, R, T> Either<L, R>.fold(left: (L) -> T, right: (R) -> T): T =
    when (this) {
        is Either.Left -> left(value)
        is Either.Right -> right(value)
    }

inline fun <L, R, T> Either<L, R>.flatMap(f: (R) -> Either<L, T>): Either<L, T> =
    fold(left = { this as Either.Left }, right = f)

inline fun <L, R, T> Either<L, R>.map(f: (R) -> T): Either<L, T> =
    flatMap { Either.Right(f(it)) }
