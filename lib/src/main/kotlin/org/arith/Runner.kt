package org.arith

import org.arith.utils.*
import kotlin.math.roundToInt

typealias Error = Pair<UInt, String>
typealias ErrorMessage = String

private data class Parser(
    val output: MutableList<Token> = mutableListOf(),
    val operators: MutableList<Token> = mutableListOf(),
    var result: Either<Error, MutableList<Token>> = right(output)
)

private fun Parser.hadError(): Boolean = result is Left<Error>

fun calculate(source: String): Either<ErrorMessage, Result> =
    organize(Lexer(source)).fold({
        left("Line(1,${it.first}): ${it.second}")
    }, ::onRight)

private fun onRight(stack: MutableList<Token>): Either<ErrorMessage, Result> {
    val args = mutableListOf<Result>()
    val output = mutableListOf<Result>()

    stack.forEach {
        when (it.type) {
            TokenType.Number -> output.add(it.lexeme!!.toDouble())

            TokenType.Star,
            TokenType.Hat,
            TokenType.Plus,
            TokenType.Minus,
            TokenType.Slash,
            TokenType.Modulo -> {
                onOperator(output, it).let { result ->
                    if (result is Left<ErrorMessage>) {
                        return@onRight result
                    }
                }
            }

            TokenType.Function -> {
                onFunction(it, args, output).let { result ->
                    if (result is Left<ErrorMessage>) {
                        return@onRight result
                    }
                }
            }

            else -> return@onRight left("Line(1,${it.column + 1u}): '${it.lexeme}'")
        }
    }

    return if (output.size == 1) right(output.removeLast()) else
        left("Line(1,${stack.last().column + 1u}): Too many operators, too few operands")
}

@Suppress("UNCHECKED_CAST")
private fun onOperator(output: MutableList<Result>, currentToken: Token): Either<ErrorMessage, Unit> {
    val errorMessage = "Line(1,${currentToken.column + 1u}): Too many operators, too few operands"

    when (val op = currentToken.function) {
        is Unary<*> -> {
            val right = output.removeLastOrNull() ?: return left(errorMessage)
            output.add((op as Unary<Double>)(right))
        }

        is Binary<*> -> {
            val right = output.removeLastOrNull() ?: return left(errorMessage)
            val left = output.removeLastOrNull() ?: return left(errorMessage)

            if (currentToken.type == TokenType.Slash && right.roundToInt() == 0) {
                return left("Line(1,${currentToken.column + 1u}): Division by zero!!")
            }

            output.add((op as Binary<Result>)(left, right))
        }

        is Func, null -> left("Line(1,${currentToken.column + 1u}): Syntax error unexpected '${currentToken.lexeme}'")
    }

    return right(Unit)
}

private fun onFunction(
    entry: Token,
    args: MutableList<Result>,
    output: MutableList<Result>
): Either<ErrorMessage, Unit> {
    val (func, arity) = (entry.function as Func).data

    if (output.size.toUInt() < arity) {
        return left("Line(1,${entry.column + 1u}): Too few arguments to function, '${entry.lexeme}'")
    }

    repeat(arity.toInt()) {
        val top = output.removeLastOrNull()
            ?: return@onFunction left("Line(1,${entry.column + 1u}): Too few arguments to function, '${entry.lexeme}'")

        args.add(top)
    }

    val result = func(args)
    output.add(result)
    args.clear()
    return right(Unit)
}

private fun organize(lexer: Lexer): Either<Error, MutableList<Token>> {
    var parser = Parser()

    var copy = lexer.copy()

    if (copy.source.isEmpty()) {
        parser.output.add(Token("0", TokenType.Number, 0u))
        return parser.result
    }

    do {
        val (token, next) = lex(copy)

        if (token.type == TokenType.End) {
            break
        }

        parser = onTokenType(parser, token)

        if (parser.hadError()) {
            break
        }

        copy = next
    } while (true)

    while (parser.operators.isNotEmpty()) {
        val top = parser.operators.last()

        if (top.type == TokenType.LeftParen) {
            parser.result = left(top.column + 1u to "Mismatched parenthesis")
            break
        }

        parser.output.add(parser.operators.removeLast())
    }

    return parser.result
}

private fun onTokenType(parser: Parser, token: Token): Parser {
    val copy = parser.copy()

    return when (token.type) {
        TokenType.Number -> copy.let {
            it.output.add(token)
            it
        }

        TokenType.Function -> copy.let {
            it.operators.add(token)
            it
        }

        TokenType.Star,
        TokenType.Hat,
        TokenType.Plus,
        TokenType.Minus,
        TokenType.Slash,
        TokenType.Modulo -> onOp(parser, token)

        TokenType.Comma -> onComma(parser)

        TokenType.LeftParen -> copy.let {
            it.operators.add(token)
            it
        }


        TokenType.RightParen -> onRightParen(copy, token)

        else -> copy
    }

}


private fun onOp(parser: Parser, currentToken: Token): Parser {
    val copy = parser.copy()

    val removeFromOps = {
        copy.operators.isNotEmpty() &&
                copy.operators.let {
                    val top = it.last()
                    top.type != TokenType.LeftParen &&
                            (top.precedence > currentToken.precedence ||
                                    (top.precedence == currentToken.precedence &&
                                            currentToken.leftAssociative))
                }
    }

    while (removeFromOps()) {
        copy.output.add(copy.operators.removeLast())
    }

    copy.operators.add(currentToken)
    return copy
}

private fun onComma(parser: Parser): Parser {
    val copy = parser.copy()

    while (copy.operators.isNotEmpty() && copy.operators.last().type != TokenType.LeftParen) {
        copy.output.add(copy.operators.removeLast())
    }

    return copy
}

private fun onRightParen(parser: Parser, token: Token): Parser {
    val copy = parser.copy()

    if (copy.operators.isEmpty()) {
        copy.result = left(token.column + 1u to "Mismatched parenthesis")
        return copy
    }

    while (copy.operators.isNotEmpty() && copy.operators.last().type != TokenType.LeftParen) {
        if (copy.operators.isEmpty()) {
            copy.result = left(parser.operators.last().column + 1u to "Mismatched parenthesis")
            break
        }

        copy.output.add(copy.operators.removeLast())
    }

    if (copy.operators.isEmpty()) {
        copy.result = left(token.column + 1u to "Mismatched parenthesis")
        return copy
    }

    val top = copy.operators.last()

    if (top.type == TokenType.Comma) {
        copy.result = left(top.column + 1u to "Empty argument")
    }

    if (top.type == TokenType.LeftParen) {
        copy.operators.removeLast()
    }

    if (copy.operators.isNotEmpty() && copy.operators.last().type == TokenType.Function) {
        copy.output.add(copy.operators.removeLast())
    }

    return copy
}