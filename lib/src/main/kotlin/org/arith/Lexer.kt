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
val Functions: Map<String, Pair<Calculation, Arity>> =
    mapOf(
        "abs" to (atLeastNArgs("abs") { args: List<Double> -> abs(args.first()) } to Arity(1)),
        "acos" to (atLeastNArgs("acos") { args: List<Double> -> acos(args.first()) } to Arity(1)),
        "acot" to (atLeastNArgs("acot") { args: List<Double> -> atan(reciprocal(args.first())) } to Arity(1)),
        "acsc" to (atLeastNArgs("acsc") { args: List<Double> -> asin(reciprocal(args.first())) } to Arity(1)),
        "asec" to (atLeastNArgs("asec") { args: List<Double> -> acos(reciprocal(args.first())) } to Arity(1)),
        "asin" to (atLeastNArgs("asin") { args: List<Double> -> asin(args.first()) } to Arity(1)),
        "atan" to (atLeastNArgs("atan") { args: List<Double> -> atan(args.first()) } to Arity(1)),
        "ceil" to (atLeastNArgs("ceil") { args: List<Double> -> ceil(args.first()) } to Arity(1)),
        "cos" to (atLeastNArgs("cost") { args: List<Double> -> cos(args.first()) } to Arity(1)),
        "cosh" to (atLeastNArgs("cosh") { args: List<Double> -> cosh(args.first()) } to Arity(1)),
        "cot" to (atLeastNArgs("cot") { args: List<Double> -> reciprocal(tan(args.first())) } to Arity(1)),
        "csc" to (atLeastNArgs("csc") { args: List<Double> -> reciprocal(sin(args.first())) } to Arity(1)),
        "exp" to (atLeastNArgs("exp") { args: List<Double> -> exp(args.first()) } to Arity(1)),
        "exp2" to (atLeastNArgs("exp2") { args: List<Double> -> 2.0.pow(args.first()) } to Arity(1)),
        "ln" to (atLeastNArgs("ln") { args: List<Double> -> ln(args.first()) } to Arity(1)),
        "log" to (atLeastNArgs("log", n = 2) { args: List<Double> -> ln(args.first()) / ln(args[1]) } to Arity(2)),
        "log10" to (atLeastNArgs("log10") { args: List<Double> -> log10(args.first()) } to Arity(1)),
        "rad" to (atLeastNArgs("rad") { args: List<Double> -> Math.toRadians(args.first()) } to Arity(1)),
        "round" to (atLeastNArgs("round") { args: List<Double> -> round(args.first()) } to Arity(1)),
        "sec" to (atLeastNArgs("sec") { args: List<Double> -> reciprocal(cos(args.first())) } to Arity(1)),
        "sin" to (atLeastNArgs("sin") { args: List<Double> -> sin(args.first()) } to Arity(1)),
        "sinh" to (atLeastNArgs("sinh") { args: List<Double> -> sinh(args.first()) } to Arity(1)),
        "sqrt" to (atLeastNArgs("sqrt") { args: List<Double> -> sqrt(args.first()) } to Arity(1)),
        "tan" to (atLeastNArgs("tan") { args: List<Double> -> tan(args.first()) } to Arity(1)),
        "tanh" to (atLeastNArgs("tanh") { args: List<Double> -> tanh(args.first()) } to Arity(1)),
        "deg" to (atLeastNArgs("deg") { args: List<Double> -> Math.toDegrees(args.first()) } to Arity(1)),
        "floor" to (atLeastNArgs("floor") { args: List<Double> -> floor(args.first()) } to Arity(1)),
        "nroot" to
                (atLeastNArgs("nroot", n = 2) { args: List<Double> ->
                    args.first().pow(1.0 / args[1])
                } to Arity(2))
    )

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

private fun isUnaryOp(lastTokenWasOp: Boolean, column: Int, head: Char, source: String): Boolean =
    ((lastTokenWasOp && head == '-') ||
            (head == '-' && source.length >= 2 && (source[1] == '(' ||
                    column == 0 && source[1].isDigit()
                    ))
            )

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
        'a', 'c', 'e', 'l', 'r', 's', 't', 'd', 'f', 'n' -> parseFunction(lexer)
            ?: ("Unknown function name" to TokenType.ERROR)

        else ->
            if (lexer.source.first().isDigit()) {
                lexer.source.takeWhile { it.isDigit() || it == '.' } to TokenType.NUM
            } else {
                TOKEN_ERROR to TokenType.ERROR
            }
    }

private fun parseFunction(lexer: Lexer) =
    when (lexer.source.lowercase().first()) {
        'a' ->
            findFunctionName(listOf("abs", "acos", "acot", "acsc", "asec", "asin", "atan"), lexer.source)

        'c' ->
            findFunctionName(listOf("ceil", "cosh", "cos", "cot", "csc"), lexer.source)

        'e' ->
            findFunctionName(listOf("exp2", "exp"), lexer.source)

        'l' ->
            findFunctionName(
                listOf(
                    "ln",
                    "log10",
                    "log"
                ), lexer.source
            )

        'r' ->
            findFunctionName(listOf("rad", "round"), lexer.source)

        's' ->
            findFunctionName(listOf("sec", "sinh", "sin", "sqrt"), lexer.source)

        't' ->
            findFunctionName(listOf("tanh", "tan"), lexer.source)

        else -> {
            findFunctionName(listOf("deg", "floor", "nroot"), lexer.source)
        }
    }

private fun findFunctionName(names: List<String>, source: String): Pair<String, TokenType>? =
    names.firstOrNull {
        source.take(it.length) == it
    }?.let { it to TokenType.FUNCTION }


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

fun isOp(tokenType: TokenType): Boolean =
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
