package org.arith

import org.arith.utils.Binary
import org.arith.utils.Func
import org.arith.utils.Relation
import org.arith.utils.Result
import kotlin.math.*

private const val TOKEN_ERROR: String = "Unknown token"

enum class Precedence {
    None,
    Term, // + -
    Factor, // * / % ^
    Unary, // ! -
}

enum class TokenType {
    Star,
    Hat,
    Plus,
    Minus,
    Slash,
    Modulo,
    LeftParen,
    RightParen,
    Comma,
    Number,
    Function,
    End,
    Error
}

data class Token(
    val lexeme: String?,
    val type: TokenType,
    val column: UInt,
    val precedence: Precedence = Precedence.None,
    val leftAssociative: Boolean = false,
    val function: Relation? = null
)

data class Lexer(
    val source: String,
    val column: UInt = 0u,
    val lastTokenWasOp: Boolean = false,
    val reachedEnd: Boolean = false,
)

val mul = Binary<Result> { left, right -> left * right }
val power = Binary<Result> { left, right -> left.pow(right) }
val plus = Binary<Result> { left, right -> left + right }
val sub = Binary<Result> { left, right -> left - right }
val div = Binary<Result> { left, right -> left / right }
val modulo = Binary<Result> { left, right -> left % right }

val Functions: Map<String, Func> =
    run {
        val reciprocal = { n: Double -> 1.0 / n }

        mapOf(
            "!" to Func({ args: List<Double> ->
                fun factorial(n: Result): Double = if (n.toInt() == 1) 1.0 else (n * factorial(n - 1))
                factorial(args.first())
            } to 1u),
            "-" to Func({ args: List<Double> ->
                -args.first()
            } to 1u),
            "abs" to Func({ args: List<Double> -> abs(args.first()) } to 1u),
            "acos" to Func({ args: List<Double> -> acos(args.first()) } to 1u),
            "acot" to Func({ args: List<Double> -> atan(reciprocal(args.first())) } to 1u),
            "acsc" to Func({ args: List<Double> -> asin(reciprocal(args.first())) } to 1u),
            "asec" to Func({ args: List<Double> -> acos(reciprocal(args.first())) } to 1u),
            "asin" to Func({ args: List<Double> -> asin(args.first()) } to 1u),
            "atan" to Func({ args: List<Double> -> atan(args.first()) } to 1u),
            "ceil" to Func({ args: List<Double> -> ceil(args.first()) } to 1u),
            "cos" to Func({ args: List<Double> -> cos(args.first()) } to 1u),
            "cosh" to Func({ args: List<Double> -> cosh(args.first()) } to 1u),
            "cot" to Func({ args: List<Double> -> reciprocal(tan(args.first())) } to 1u),
            "csc" to Func({ args: List<Double> -> reciprocal(sin(args.first())) } to 1u),
            "exp" to Func({ args: List<Double> -> exp(args.first()) } to 1u),
            "exp2" to Func({ args: List<Double> -> 2.0.pow(args.first()) } to 1u),
            "ln" to Func({ args: List<Double> -> ln(args.first()) } to 1u),
            "log" to Func({ args: List<Double> -> ln(args.first()) / ln(args[1]) } to 2u),
            "log10" to Func({ args: List<Double> -> log10(args.first()) } to 1u),
            "rad" to Func({ args: List<Double> -> Math.toRadians(args.first()) } to 1u),
            "round" to Func({ args: List<Double> -> round(args.first()) } to 1u),
            "sec" to Func({ args: List<Double> -> reciprocal(cos(args.first())) } to 1u),
            "sin" to Func({ args: List<Double> -> sin(args.first()) } to 1u),
            "sinh" to Func({ args: List<Double> -> sinh(args.first()) } to 1u),
            "sqrt" to Func({ args: List<Double> -> sqrt(args.first()) } to 1u),
            "tan" to Func({ args: List<Double> -> tan(args.first()) } to 1u),
            "tanh" to Func({ args: List<Double> -> tanh(args.first()) } to 1u),
            "deg" to Func({ args: List<Double> -> Math.toDegrees(args.first()) } to 1u),
            "floor" to Func({ args: List<Double> -> floor(args.first()) } to 1u),
            "nroot" to
                    Func({ args: List<Double> ->
                        args.first().pow(1.0 / args[1])
                    } to 2u)
        )
    }

fun wasEmpty(lexer: Lexer): Boolean = !lexer.reachedEnd && lexer.column == 0u && lexer.source.isEmpty()

fun lex(lexer: Lexer): Pair<Token, Lexer> {
    if (wasEmpty(lexer)) {
        return Token("0", TokenType.Number, 0u) to lexer
    } else if (lexer.source.isEmpty()) {
        return Token(null, TokenType.End, lexer.column, Precedence.None) to lexer.copy(reachedEnd = true)
    }

    val lexerCopy = skipWhiteSpace(lexer.copy())

    val isUnaryMinus = isUnaryMinus(lexerCopy)

    val (lexeme, tokenType) = parse(lexerCopy, isUnaryMinus)

    if (tokenType == TokenType.Number && lexeme.count { it == '.' } > 1) {
        return Token(
            "Syntax error: floating point number cannot contain more than one '.'",
            TokenType.Error,
            lexer.column,
            Precedence.None
        ) to lexerCopy
    } else if (tokenType == TokenType.Error) {
        return Token(lexeme, TokenType.Error, lexer.column, Precedence.None) to lexerCopy
    }

    val func =
        when (tokenType) {
            TokenType.Function -> Functions[lexeme]
            else -> if (isOp(lexeme, tokenType, isUnaryMinus)) fromOp(tokenType) else null
        }

    return Token(
        lexeme,
        tokenType,
        lexerCopy.column,
        precedenceOf(lexeme, isUnaryMinus),
        isLeftAssociative(lexeme, tokenType, isUnaryMinus),
        func
    ) to
            lexerCopy.copy(
                source = lexerCopy.source.drop(lexeme.length),
                column = lexerCopy.column + lexeme.length.toUInt(),
                lastTokenWasOp = isOp(lexeme, tokenType, isUnaryMinus)
            )
}

private fun isUnaryMinus(lexer: Lexer): Boolean {
    val (source, column, lastTokenWasOp) = lexer
    val head = source.first()

    return (lastTokenWasOp && head == '-') ||
            (head == '-' && source.length >= 2 && (source[1] == '(' ||
                    column == 0u && (source[1].isDigit() || source[1] == '-')
                    ))
}

private fun fromOp(tokenType: TokenType): Relation? =
    when (tokenType) {
        TokenType.Star -> mul
        TokenType.Hat -> power
        TokenType.Plus -> plus
        TokenType.Minus -> sub
        TokenType.Slash -> div
        TokenType.Modulo -> modulo
        else -> null
    }

private fun isLeftAssociative(lexeme: String, tokenType: TokenType, isUnaryMinus: Boolean) =
    isOp(lexeme, tokenType, isUnaryMinus) && lexeme != "^" && lexeme != "("

private fun parse(lexer: Lexer, isUnaryMinus: Boolean): Pair<String, TokenType> =
    when (val head = lexer.source.first().lowercaseChar()) {
        '!' -> "!" to TokenType.Function
        '*' -> "*" to TokenType.Star
        '^' -> "^" to TokenType.Hat
        '+' -> "+" to TokenType.Plus
        '-' -> "-" to (if (isUnaryMinus) TokenType.Function else TokenType.Minus)
        '%' -> "%" to TokenType.Modulo
        '/' -> "/" to TokenType.Slash
        '(' -> "(" to TokenType.LeftParen
        ',' -> "," to TokenType.Comma
        ')' -> ")" to TokenType.RightParen
        'a', 'd', 'c', 'e', 'f', 'l', 'n', 'p', 'r', 's', 't' -> {
            if (head == 'e' && (lexer.source.length == 1 || isNotExpFunctions(lexer.source))) {
                Math.E.toString() to TokenType.Number
            } else if (lexer.source.length >= 2 && lexer.source.take("pi".length) == "pi") {
                Math.PI.toString() to TokenType.Number
            } else
                parseFunction(lexer)
                    ?: ("Unknown function name" to TokenType.Error)
        }

        else ->
            if (lexer.source.first().isDigit()) {
                lexer.source.takeWhile { it.isDigit() || it == '.' } to TokenType.Number
            } else {
                TOKEN_ERROR to TokenType.Error
            }
    }

private inline fun isNotExpFunctions(source: String): Boolean {
    val length = source.length

    return (length >= "exp".length && source.take("exp".length) != "exp") || (length >= "exp2".length && source.take("exp2".length) != "exp2")
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

private inline fun findFunctionName(names: List<String>, source: String): Pair<String, TokenType>? =
    names.firstOrNull {
        source.take(it.length) == it
    }?.let { it to TokenType.Function }

private fun precedenceOf(lexeme: String, isUnary: Boolean): Precedence =
    when (lexeme) {
        "+" -> Precedence.Term
        "*", "/", "%", "^" -> Precedence.Factor
        "!" -> Precedence.Unary
        else ->
            if (lexeme == "-" && isUnary)
                Precedence.Unary
            else if (lexeme == "-") Precedence.Term
            else Precedence.None
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
                                (lexer.source.length - it.length).absoluteValue.toUInt()
                    )
                }
            }

            else -> lexer
        }
    return result
}

fun isOp(lexeme: String, tokenType: TokenType, isUnaryMinus: Boolean): Boolean =
    when (tokenType) {
        TokenType.Star,
        TokenType.Hat,
        TokenType.Plus,
        TokenType.Slash,
        TokenType.Modulo,
        TokenType.LeftParen -> true

        else -> (isUnaryMinus && lexeme == "-") || (tokenType == TokenType.Minus) || (lexeme == "!")
    }
