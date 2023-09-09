package org.arith

import org.arith.utils.atLeastNValues
import org.arith.utils.guard
import org.arith.utils.left
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class FuncsTest private constructor() {
    @Test
    fun givenLessThanNValuesReturnsFalse() {
        assertEquals(false, atLeastNValues<Int>(10)(listOf(1)))
    }

    @Test
    fun givenAFalseValueReturnsLeft() {
        val actual =
            guard<Int>(
                "givenAFalseValueReturnsLeft" to "errors on predicate",
                { it.first() == 11 },
                { it.size })(listOf(10))

        assertEquals(
            left("givenAFalseValueReturnsLeft: errors on predicate"),
            actual
        )
    }
}