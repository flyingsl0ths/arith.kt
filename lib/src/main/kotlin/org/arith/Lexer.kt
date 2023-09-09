package org.arith

import kotlin.math.*
import org.arith.utils.Either
import org.arith.utils.atLeastNArgs

enum class Precedence {
    NONE,
    TERM, // + -
    FACTOR, // * / %
    UNARY, // ! - ^
}

data class Token(
        val data: String?,
        val type: TokenType,
        val column: Int,
        val precedence: Precedence = Precedence.NONE,
        val leftAssociative: Boolean = false,
        val function: ((List<Double>) -> Either<String, Double>)? = null
)

data class Lexer(var source: String, var column: Int = 0, var lastTokenWasOp: Boolean = false)

enum class TokenType {
    BANG,
    STAR,
    POW,
    PLUS,
    MINUS,
    SLASH,
    MOD,
    LEFT_PAREN,
    RIGHT_PAREN,
    COMMA,
    NUM,
    FUNCTION,
    EOF,
    ERROR
}

private const val TOKEN_ERROR: String = "Unknown token"

fun lex(lexer: Lexer): Pair<Token, Lexer> {
    if (lexer.source.isEmpty()) {
        return Token(null, TokenType.EOF, lexer.column, Precedence.NONE) to lexer
    }

    val lexerCopy = skipWhiteSpace(lexer.copy())

    val head = lexerCopy.source.first()

    val isUnaryMinus =
            (lexerCopy.source.length >= 2) &&
                    ((lexer.lastTokenWasOp && head == '-' && lexerCopy.source[1].isDigit()) ||
                            (head == '-' && lexerCopy.source[1].isDigit()))

    val (lexeme, tokenType) = parse(lexerCopy)

    if (tokenType == TokenType.NUM && lexeme.count { it == '.' } > 1) {
        return Token(
                "Syntax error: floating point number cannot contain more than one '.'",
                TokenType.ERROR,
                lexer.column,
                Precedence.NONE
        ) to lexerCopy
    } else if (tokenType == TokenType.ERROR) {
        return Token(lexeme, TokenType.ERROR, lexer.column, Precedence.NONE) to lexerCopy
    }

    val result =
            when (lexeme) {
                TOKEN_ERROR ->
                        Token("Unknown lexeme: $head", tokenType, lexer.column, Precedence.NONE) to
                                lexerCopy
                else ->
                        Token(
                                lexeme,
                                tokenType,
                                lexerCopy.column,
                                if (isUnaryMinus) Precedence.UNARY else precedenceOf(lexeme),
                                isLeftAssociative(tokenType, lexeme),
                                if (tokenType == TokenType.FUNCTION) fromFunctionName(lexeme)
                                else null
                        ) to
                                lexerCopy.copy(
                                        source = lexerCopy.source.drop(lexeme.length),
                                        column = lexerCopy.column + lexeme.length,
                                        lastTokenWasOp = isOp(tokenType)
                                )
            }

    return result
}

private fun isLeftAssociative(tokenType: TokenType, lexeme: String) =
        isOp(tokenType) && lexeme != "^"

private fun parse(lexer: Lexer): Pair<String, TokenType> =
        when (lexer.source.first().lowercaseChar()) {
            '!' -> "!" to TokenType.BANG
            '*' -> "*" to TokenType.STAR
            '^' -> "^" to TokenType.POW
            '+' -> "+" to TokenType.PLUS
            '-' -> "-" to TokenType.MINUS
            '%' -> "%" to TokenType.MOD
            '/' -> "/" to TokenType.SLASH
            '(' -> "(" to TokenType.LEFT_PAREN
            ',' -> "," to TokenType.COMMA
            ')' -> ")" to TokenType.RIGHT_PAREN
            in 'a'..'z' -> parseFunction(lexer) ?: ("Unknown function name" to TokenType.ERROR)
            else ->
                    if (lexer.source.first().isDigit()) {
                        lexer.source.takeWhile { it.isDigit() || it == '.' } to TokenType.NUM
                    } else {
                        TOKEN_ERROR to TokenType.ERROR
                    }
        }

private fun parseFunction(lexer: Lexer) =
        when (lexer.source.first()) {
            'a' ->
                    listOf("abs", "acos", "acot", "acsc", "asec", "asin", "atan")
                            .firstOrNull { lexer.source.take(it.length) == it }
                            ?.let { it to TokenType.FUNCTION }
            'c' ->
                    if (lexer.source.last() == 'h') {
                        "cosh" to TokenType.FUNCTION
                    } else {
                        listOf("ceil", "cos", "cosh", "cot", "csc")
                                .firstOrNull { lexer.source.take(it.length) == it }
                                ?.let { it to TokenType.FUNCTION }
                    }
            'e' ->
                    (if (lexer.source.last() == '2') {
                        "exp2"
                    } else {
                        "exp"
                    }) to TokenType.FUNCTION
            'l' ->
                    if (lexer.source.last() == '0') {
                        "log10" to TokenType.FUNCTION
                    } else {
                        listOf(
                                        "ln",
                                        "log",
                                        "log10",
                                )
                                .firstOrNull { lexer.source.take(it.length) == it }
                                ?.let { it to TokenType.FUNCTION }
                    }
            'r' ->
                    listOf("rad", "round").firstOrNull { lexer.source.take(it.length) == it }?.let {
                        it to TokenType.FUNCTION
                    }
            's' ->
                    if (lexer.source.last() == 'h') {
                        "sinh" to TokenType.FUNCTION
                    } else {
                        listOf("sec", "sin", "sqrt")
                                .firstOrNull { lexer.source.take(it.length) == it }
                                ?.let { it to TokenType.FUNCTION }
                    }
            't' ->
                    (if (lexer.source.last() == 'h') {
                        "tanh"
                    } else {
                        "tan"
                    }) to TokenType.FUNCTION
            else -> {
                listOf("deg", "floor", "nroot")
                        .firstOrNull { lexer.source.take(it.length) == it }
                        ?.let { it to TokenType.FUNCTION }
            }
        }

private fun fromFunctionName(functionName: String): ((List<Double>) -> Either<String, Double>)? =
        when (functionName) {
            "abs" -> atLeastNArgs("abs") { args: List<Double> -> abs(args.first()) }
            "acos" -> atLeastNArgs("acos") { args: List<Double> -> acos(args.first()) }
            "acot" -> atLeastNArgs("acot") { args: List<Double> -> atan(1.0 / args.first()) }
            "acsc" -> atLeastNArgs("acsc") { args: List<Double> -> asin(1.0 / args.first()) }
            "asec" -> atLeastNArgs("asec") { args: List<Double> -> acos(1.0 / args.first()) }
            "asin" -> atLeastNArgs("asin") { args: List<Double> -> asin(args.first()) }
            "atan" -> atLeastNArgs("atan") { args: List<Double> -> atan(args.first()) }
            "ceil" -> atLeastNArgs("ceil") { args: List<Double> -> ceil(args.first()) }
            "cos" -> atLeastNArgs("cost") { args: List<Double> -> cos(args.first()) }
            "cosh" -> atLeastNArgs("cosh") { args: List<Double> -> cosh(args.first()) }
            "cot" -> atLeastNArgs("cot") { args: List<Double> -> reciprocal(tan(args.first())) }
            "csc" -> atLeastNArgs("csc") { args: List<Double> -> reciprocal(sin(args.first())) }
            "exp" -> atLeastNArgs("exp") { args: List<Double> -> exp(args.first()) }
            "exp2" -> atLeastNArgs("exp2") { args: List<Double> -> 2.0.pow(args.first()) }
            "ln" -> atLeastNArgs("ln") { args: List<Double> -> ln(args.first()) }
            "log" -> atLeastNArgs("log", 2) { args: List<Double> -> ln(args.first()) / ln(args[1]) }
            "log10" -> atLeastNArgs("log10") { args: List<Double> -> log10(args.first()) }
            "rad" -> atLeastNArgs("rad") { args: List<Double> -> Math.toRadians(args.first()) }
            "round" -> atLeastNArgs("round") { args: List<Double> -> round(args.first()) }
            "sec" -> atLeastNArgs("sec") { args: List<Double> -> reciprocal(cos(args.first())) }
            "sin" -> atLeastNArgs("sin") { args: List<Double> -> sin(args.first()) }
            "sinh" -> atLeastNArgs("sinh") { args: List<Double> -> sinh(args.first()) }
            "sqrt" -> atLeastNArgs("sqrt") { args: List<Double> -> sqrt(args.first()) }
            "tan" -> atLeastNArgs("tan") { args: List<Double> -> tan(args.first()) }
            "tanh" -> atLeastNArgs("tanh") { args: List<Double> -> tanh(args.first()) }
            "deg" -> atLeastNArgs("deg") { args: List<Double> -> Math.toDegrees(args.first()) }
            "floor" -> atLeastNArgs("floor") { args: List<Double> -> floor(args.first()) }
            "nroot" ->
                    atLeastNArgs("nroot", 2) { args: List<Double> ->
                        args.first().pow(1.0 / args[1])
                    }
            else -> null
        }

private fun reciprocal(n: Double): Double = 1.0 / n

private fun precedenceOf(lexeme: String): Precedence =
        when (lexeme) {
            "+", "-" -> Precedence.TERM
            "*", "/", "%" -> Precedence.FACTOR
            "!", "^" -> Precedence.FACTOR
            else -> Precedence.NONE
        }

private fun skipWhiteSpace(lexer: Lexer): Lexer {
    val result =
            when (lexer.source.first()) {
                ' ', '\t' -> {
                    lexer.source.dropWhile { it == ' ' || it == '\t' }.let {
                        lexer.copy(
                                source = it,
                                column =
                                        lexer.column +
                                                (lexer.source.length - it.length).absoluteValue
                        )
                    }
                }
                else -> lexer
            }

    return result
}

private fun isOp(tokenType: TokenType): Boolean =
        when (tokenType) {
            TokenType.BANG,
            TokenType.STAR,
            TokenType.POW,
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.SLASH,
            TokenType.MOD -> true
            else -> false
        }
