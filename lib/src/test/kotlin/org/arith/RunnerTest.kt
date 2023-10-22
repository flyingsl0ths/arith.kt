package org.arith

import org.arith.utils.fold
import org.junit.jupiter.api.Test
import kotlin.math.abs
import kotlin.math.pow
import kotlin.math.sin
import kotlin.test.assertEquals

class RunnerTest private constructor() {
    @Test
    fun givenAnEmptySourceReturnsZero() {
        calculate("").fold(
            { assert(false) { it } }, { assertEquals(0, it.toInt()) })
    }

    @Test
    fun givenANumberReturnsANumber() {
        calculate("1").fold(
            { assert(false) { it } }, { assertEquals(1, it.toInt()) })
    }

    @Test
    fun givenANegativeNumberReturnsANumber() {
        calculate("-1").fold(
            { assert(false) { it } }, { assertEquals(-1, it.toInt()) })
    }

    @Test
    fun givenAnyBinaryOperatorReturnsTheCorrectResult() {
        val makeTestCase = { binaryOp: Char, expected: Int ->
            calculate("1${binaryOp}1").fold(
                { result -> assert(false) { result } }, { result -> assertEquals(expected, result.toInt()) })
        }

        makeTestCase('+', 2)
        makeTestCase('-', 0)
        makeTestCase('*', 1)
        makeTestCase('/', 1)
        makeTestCase('%', 0)
        makeTestCase('^', 1)
    }

    @Test
    fun givenADoubleNegationReturnTheCorrectResult() {
        calculate("--1").fold(
            { result -> assert(false) { result } }, { result -> assertEquals(1, result.toInt()) })
    }

    @Test
    fun givenAnExprContainingAParenthesisReturnsTheCorrectResult() {
        calculate("1 + (-1 * 100)").fold(
            { result -> assert(false) { result } }, { result -> assertEquals(1 + (-1 * 100), result.toInt()) })
    }

    @Test
    fun givenAFunctionReturnsTheCorrectResult() {
        calculate("abs(1)").fold(
            { result -> assert(false) { result } },
            { result -> assertEquals(abs(1), result.toInt()) })

        calculate("abs(1 * 2)").fold(
            { result -> assert(false) { result } },
            { result -> assertEquals(abs(2), result.toInt()) })

        calculate("abs(1 + (2 + 2))").fold(
            { result -> assert(false) { result } },
            { result -> assertEquals(abs(5), result.toInt()) })

        calculate("sin(rad(1))").fold(
            { result -> assert(false) { result } },
            { result -> assertEquals(sin(Math.toRadians(1.0)).toInt(), result.toInt()) })

        calculate("nroot(1,1)").fold(
            { result -> assert(false) { result } },
            { result -> assertEquals(((1.0).pow(1.0 / 1.0)).toInt(), result.toInt()) })
    }

    @Test
    fun giveAnExprContainingAMismatchedParenthesisReturnsAnError() {
        calculate(")1").fold(
            { result -> assertEquals(result, "Line(1,1): Mismatched parenthesis") }, {})

        calculate("-)").fold(
            { result -> assertEquals(result, "Line(1,2): Mismatched parenthesis") }, {})
    }

    @Test
    fun givenAFunctionWithTheWrongNumberOfArgsReturnsTheCorrectResult() {
        calculate("nroot(1)").fold(
            { result -> assertEquals(result, "Line(1,1): Too few arguments to function, 'nroot'") }, {})
    }

    @Test
    fun givenADivisionByZeroReturnsAnError() {
        calculate("1/0").fold({ result ->
            assertEquals(result, "Line(1,2): Division by zero!!")
        }, {})
    }
}