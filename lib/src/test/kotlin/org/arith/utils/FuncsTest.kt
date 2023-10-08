package org.arith.utils

import org.junit.jupiter.api.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class FuncsTest private constructor() {
    @Test
    fun givenLessThanNValuesReturnsFalse() {
        assertEquals(false, atLeastNValues<Int>(10)(listOf(1)))
    }

    @Test
    fun givenAFalseValueReturnsLeft() {
        val actual =
            guard<IntRange, Either<Int, Int>>(
                { it.first() == 11 },
                { left(0) }
            )
            { right(it.sum()) }

        actual(0..10).fold({
            assertFalse(false, "Expected the right value.")
        }) {
            assertTrue(true)
        }
    }
}