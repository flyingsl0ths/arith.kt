package org.arith

import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest private constructor() {
    @Test
    fun givenAnEmptySourceReturnsAnEOFToken() {
        val lexer = Lexer("")
        val (token, next) = lex(lexer)

        assertEquals(lexer, next)

        assertEquals(Token(null, TokenType.End, 0u), token)
    }

    @Test
    fun givenWhiteSpaceIgnoresIt() {
        val lexer = Lexer("   1")
        val (token, next) = lex(lexer)

        assertEquals("", next.source)
        assertEquals(4u, next.column)

        assertEquals(Token("1", TokenType.Number, 3u), token)
    }

    @Test
    fun givenAWholeNumberReturnsAWholeNumber() {
        val lexer = Lexer("1")
        val (token, next) = lex(lexer)

        assertEquals("", next.source)
        assertEquals(1u, next.column)

        assertEquals(Token("1", TokenType.Number, 0u), token)
    }

    @Test
    fun givenADecimalReturnsADecimal() {
        val lexer = Lexer("1.0")
        val (token, next) = lex(lexer)

        assertEquals("", next.source)
        assertEquals(3u, next.column)

        assertEquals(Token("1.0", TokenType.Number, 0u), token)
    }

    @Test
    fun givenAnySingleCharacterReturnsTheAppropriateResult() {
        fun opPrecedence(lexeme: String): Precedence =
            when (lexeme) {
                "+" -> Precedence.Term
                "*", "/", "%", "^" -> Precedence.Factor
                "!" -> Precedence.Unary
                else -> Precedence.None
            }

        fun tokenTypeOfSingle(lexeme: String) =
            when (lexeme) {
                "!" -> TokenType.Function
                "*" -> TokenType.Star
                "^" -> TokenType.Hat
                "+" -> TokenType.Plus
                "/" -> TokenType.Slash
                "%" -> TokenType.Modulo
                "(" -> TokenType.LeftParen
                ")" -> TokenType.RightParen
                "," -> TokenType.Comma
                else -> TokenType.Error
            }

        "!*^+%/(,)".forEach {
            val source = it.toString()

            val lexer = Lexer(source)
            val (token, next) = lex(lexer)

            assertEquals("", next.source)
            assertEquals(1u, next.column)

            val precedence = opPrecedence(source)

            assertEquals(
                Token(
                    source,
                    tokenTypeOfSingle(source),
                    0u,
                    precedence,
                    source != "^" && precedence != Precedence.None
                ),
                token.copy(function = null)
            )
        }
    }

    @Test
    fun givenAnInvalidDecimalReturnsAnAppropriateError() {
        val lexer = Lexer("1..0")
        val (token, next) = lex(lexer)

        assertEquals("1..0", next.source)
        assertEquals(0u, next.column)

        assertEquals(
            Token(
                "Syntax error: floating point number cannot contain more than one '.'",
                TokenType.Error,
                0u,
                Precedence.None
            ),
            token.copy(function = null)
        )
    }

    @Test
    fun givenAnKnownFunctionNameReturnsAnAppropriateToken() {
        listOf(
            "abs",
            "acos",
            "acot",
            "acsc",
            "asec",
            "asin",
            "atan",
            "ceil",
            "cos",
            "cosh",
            "cot",
            "csc",
            "exp",
            "exp2",
            "ln",
            "log",
            "log10",
            "rad",
            "round",
            "sec",
            "sin",
            "sinh",
            "sqrt",
            "tan",
            "tanh",
            "deg",
            "floor",
            "nroot"
        )
            .forEach {
                val lexer = Lexer(it)
                val (token, next) = lex(lexer)

                assertEquals("", next.source)
                assertEquals(it.length.toUInt(), next.column)

                assertEquals(
                    Token(it, TokenType.Function, 0u, Precedence.None),
                    token.copy(function = null)
                )
            }
    }

    @Test
    fun givenAnUnknownFunctionNameReturnsAnError() {
        val lexer = Lexer("abc")
        val (token, next) = lex(lexer)

        assertEquals("abc", next.source)
        assertEquals(0u, next.column)

        assertEquals(Token("Unknown function name", TokenType.Error, 0u, Precedence.None), token)
    }

    @Test
    fun givenAnUnknownLexemeReturnsAnError() {
        val lexer = Lexer(">")
        val (token, next) = lex(lexer)

        assertEquals(">", next.source)
        assertEquals(0u, next.column)

        assertEquals(Token("Unknown token", TokenType.Error, 0u, Precedence.None), token)
    }

    @Test
    fun givenANegativeNumberReturnsTheAppropriateTokens() {
        val lexer = Lexer("-10")
        val (token, next) = lex(lexer)

        assertEquals("10", next.source)
        assertEquals(1u, next.column)

        assertEquals(Token("-", TokenType.Function, 0u, Precedence.Unary, true), token.copy(function = null))
    }

    @Test
    fun givenANegativeNumberBetweenAnExprReturnsTheAppropriateTokens() {
        val lexer = Lexer("+ -10")
        val (token, next) = lex(lexer)

        assertEquals(" -10", next.source)
        assertEquals(1u, next.column)

        assertEquals(Token("+", TokenType.Plus, 0u, Precedence.Term, true), token.copy(function = null))

        val (secondToken, second) = lex(next)

        assertEquals("10", second.source)
        assertEquals(3u, second.column)

        assertEquals(Token("-", TokenType.Function, 2u, Precedence.Unary, true), secondToken.copy(function = null))
    }

    @Test
    fun givenASubtractionExprReturnsTheAppropriateResult() {
        val (token, lexer) = lex(Lexer("1-1"))

        assertEquals("-1", lexer.source)
        assertEquals(1u, lexer.column)
        assertEquals(Token("1", TokenType.Number, 0u), token)

        val (secondToken, second) = lex(lexer)

        assertEquals("1", second.source)
        assertEquals(2u, second.column)
        assertEquals(Token("-", TokenType.Minus, 1u, Precedence.Term, true), secondToken.copy(function = null))

        val (thirdToken, third) = lex(second)

        assertEquals("", third.source)
        assertEquals(3u, third.column)
        assertEquals(Token("1", TokenType.Number, 2u), thirdToken)
    }

    @Test
    fun givenAConstantReturnsTheAppropriateResult() {
        val (token, lexer) = lex(Lexer("e"))

        assertEquals(Token(Math.E.toString(), TokenType.Number, 0u), token)
        assertEquals("", lexer.source)

        val (secondToken, second) = lex(Lexer("pi"))

        assertEquals(Token(Math.PI.toString(), TokenType.Number, 0u), secondToken)
        assertEquals("", second.source)
    }
}

