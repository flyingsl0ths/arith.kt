package org.arith

import org.arith.utils.*
import java.util.*
import kotlin.math.pow

private data class Parser(
    val output: MutableList<Token> = mutableListOf(),
    val ops: MutableList<Token> = mutableListOf(),
    var result: Either<Pair<String, Int>, MutableList<Token>> = right(output)
)

private fun hadError(parser: Parser): Boolean = parser.result is Either.Left<Pair<String, Int>>

fun runLexer(lexer: Lexer): Either<String, Double> =
    organize(lexer).fold({
        left("Line(1,${it.first}): ${it.second}")
    }, ::onRight)

private fun onRight(stack: MutableList<Token>): Either<String, Double> {
    val args = mutableListOf<Double>()
    val output = mutableListOf<Double>()

    stack.forEach {
        when (it.type) {
            TokenType.NUM -> output.add(it.lexeme!!.toDouble())

            TokenType.BANG,
            TokenType.STAR,
            TokenType.POW,
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.SLASH,
            TokenType.MOD -> {
                val result = onOperator(output, it)
                if (result is Either.Left<String>) {
                    return@onRight result
                }
            }

            TokenType.FUNCTION -> {
                val result = onFunction(it, args, output)
                if (result is Either.Left<String>) {
                    return@onRight result
                }
            }


            else -> return@onRight left("Line(1,${it.column}): '${it.lexeme}'")
        }
    }

    return if (output.size == 1) right(output.removeLast()) else
        left("Line(1,${stack.last().column}): Too many operators, too few operands")
}

@Suppress("UNCHECKED_CAST")
private fun onOperator(output: MutableList<Double>, currentToken: Token): Either<String, Unit> {
    val errorMessage = "Line(1,${currentToken.column}): Too many operators, too few operands"

    when (val op = fromOp(currentToken.type, currentToken.precedence).get()) {
        is Relation.Unary<*> -> {
            val right = output.removeLastOrNull() ?: return left(errorMessage)
            output.add((op as Relation.Unary<Double>)(right))
        }

        is Relation.Binary<*> -> {
            val right = output.removeLastOrNull() ?: return left(errorMessage)
            val left = output.removeLastOrNull() ?: return left(errorMessage)

            output.add((op as Relation.Binary<Double>)(left, right))
        }
    }

    return right(Unit)
}

private fun fromOp(tokenType: TokenType, precedence: Precedence): Optional<Relation> =
    Optional.ofNullable(when (tokenType) {
        TokenType.BANG -> {
            fun factorial(n: Double): Double = if (n.toInt() == 1) 1.0 else (n * factorial(n - 1))
            Relation.Unary(::factorial)
        }

        TokenType.STAR -> Relation.Binary<Double> { left, right -> right * left }
        TokenType.POW -> Relation.Binary<Double> { left, right -> right.pow(left) }
        TokenType.PLUS -> Relation.Binary<Double> { left, right -> right + left }
        TokenType.MINUS ->
            if (precedence == Precedence.UNARY) Relation.Unary<Double> { right -> -right }
            else Relation.Binary<Double> { left, right -> right - left }

        TokenType.SLASH -> Relation.Binary<Double> { left, right -> right / left }
        TokenType.MOD -> Relation.Binary<Double> { left, right -> right % left }
        else -> null
    })

private fun onFunction(
    entry: Token,
    args: MutableList<Double>,
    output: MutableList<Double>
): Either<String, Unit> {
    val (func, arity) = entry.function!!

    if (output.size < arity.data) {
        return left("Line(1,${entry.column}): Too few arguments to function, '${entry.lexeme}'")
    }

    repeat(arity.data) {
        val top = output.removeLastOrNull()
        if (top != null) {
            args.add(top)
        } else {
            return@onFunction left("Line(1,${entry.column}): Too few arguments to function, '${entry.lexeme}'")
        }
    }

    return when (val result = func(args)) {
        is Either.Left<String> -> {
            val error = result.value
            left("Line(1,${entry.column}): $error")
        }

        is Either.Right<Double> -> {
            val calcResult = result.value
            output.add(calcResult)
            args.clear()
            right(Unit)
        }
    }
}

private fun organize(lexer: Lexer): Either<Pair<String, Int>, MutableList<Token>> {
    var parser = Parser()
    var lexer_ = lexer.copy()

    do {
        val (token, lexer__) = lex(lexer_)

        when (token.type) {
            TokenType.NUM -> parser.output.add(token)

            TokenType.FUNCTION -> parser.ops.add(token)

            TokenType.BANG,
            TokenType.STAR,
            TokenType.POW,
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.SLASH,
            TokenType.MOD -> parser = onOp(parser, token)

            TokenType.COMMA -> parser = onComma(parser)

            TokenType.LEFT_PAREN -> parser.ops.add(token)


            TokenType.RIGHT_PAREN -> when (val result = onRightParen(parser, token)) {
                is Either.Left<Boolean> -> break
                is Either.Right<Parser> -> parser = result.value
            }

            else -> break
        }

        if (hadError(parser)) {
            break
        }

        lexer_ = lexer__
    } while (true)

    while (parser.ops.isNotEmpty()) {
        val top = parser.ops.last()

        if (top.type == TokenType.LEFT_PAREN) {
            parser.result = left("Mismatched parenthesis" to top.column)
            break
        }

        parser.output.add(parser.ops.removeLast())
    }

    return parser.result
}


private fun onOp(parser: Parser, currentToken: Token): Parser {
    val parser_ = parser.copy()

    val removeFromOps = {
        parser_.ops.isNotEmpty() &&
                parser_.ops.let {
                    val last = it.last()
                    last.type != TokenType.LEFT_PAREN &&
                            (last.precedence > currentToken.precedence ||
                                    (last.precedence == currentToken.precedence &&
                                            currentToken.leftAssociative))
                }
    }

    while (removeFromOps()) {
        parser_.output.add(parser.ops.removeLast())
    }

    parser_.ops.add(currentToken)
    return parser_
}

private fun onComma(parser: Parser): Parser {
    val parser_ = parser.copy()

    while (parser_.ops.isNotEmpty() && parser.ops.last().type != TokenType.LEFT_PAREN) {
        parser_.output.add(parser.ops.removeLast())
    }

    return parser_
}

private fun onRightParen(parser: Parser, token: Token): Either<Boolean, Parser> {
    // 1 + 1 + (1)
    val parser_ = parser.copy()

    if (parser_.ops.isEmpty()) {
        parser_.result = left("Mismatched parenthesis" to token.column)
        left(true)
    }

    while (parser_.ops.last().type != TokenType.LEFT_PAREN) {
        parser_.output.add(parser.ops.removeLast())

        if (parser_.ops.isEmpty()) {
            parser_.result = left("Mismatched parenthesis" to parser.ops.last().column)
            break
        }

        val top = parser_.ops.last()

        if (top.type == TokenType.COMMA) {
            parser_.result = left("Empty argument" to top.column)
            break
        }

        if (top.type == TokenType.LEFT_PAREN) {
            parser_.ops.removeLast()
        }

        if (parser_.ops.isNotEmpty() && parser.ops.last().type == TokenType.FUNCTION) {
            parser_.output.add(parser.ops.removeLast())
        }
    }

    return right(parser_)
}