import 'dart:core';

void main() {
  List<Token> scanned = scan('15 + 22 + 1.55');
  print(scanned);
  Expression parsed = Parser(scanned).parse();
  print(parsed);
}

enum TokenType {
  // Single-character tokens.
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  COMMA,
  DOT,
  MINUS,
  PLUS,
  SEMICOLON,
  SLASH,
  STAR,

  // One or two character tokens.
  BANG,
  BANG_EQUAL,
  EQUAL,
  EQUAL_EQUAL,
  GREATER,
  GREATER_EQUAL,
  LESS,
  LESS_EQUAL,

  // Literals.
  IDENTIFIER,
  STRING,
  NUMBER,

  // Keywords.
  AND,
  CLASS,
  ELSE,
  FALSE,
  FUN,
  FOR,
  IF,
  NIL,
  OR,
  PRINT,
  RETURN,
  SUPER,
  THIS,
  TRUE,
  VAR,
  WHILE,
  EOF
}

class Token {
  final TokenType type;
  final String lexeme;
  final Object literal;
  final int line;

  Token(this.type, this.lexeme, this.literal, this.line);

  @override
  String toString() {
    return '{$type: $literal}';
  }
}

final RegExp _numeric = RegExp(r'^-?[0-9]+$');

bool isNumber(String str) {
  return _numeric.hasMatch(str);
}

class ScanException implements Exception {}

List<Token> scan(String text) {
  List<Token> tokens = [];
  for (int current = 0; current < text.length; current++) {
    String character = text[current];
    bool isLastCharacter = current <= text.length - 1;

    switch (character) {
      case '"':
        int start = current;
        int end = current + 1;
        while (end < text.length && text[end] != '"') {
          end++;
        }
        String value = text.substring(start + 1, end);
        tokens.add(Token(TokenType.STRING, value, value, 0));
        current = end + 1;
        break;
      case "/n":
        break;
      case "+":
        tokens.add(Token(TokenType.PLUS, "+", "+", 0));
        break;
      case "(":
        tokens.add(Token(TokenType.LEFT_PAREN, "(", "(", 0));
        break;
      case ")":
        tokens.add(Token(TokenType.RIGHT_PAREN, ")", ")", 0));
        break;
      case "<":
        if (!isLastCharacter) {
          String nextCharacter = text[current + 1];
          if (nextCharacter == "=") {
            tokens.add(Token(TokenType.LESS_EQUAL, "<=", "<=", 0));
            current++;
            break;
          }
        }

        tokens.add(Token(TokenType.LESS, "<", "<", 0));
        break;
      case " ":
        break;
      default:
        if (!isNumber(character)) throw ScanException();

        int start = current;
        int end = current + 1;
        bool dotReached = false;

        while (end < text.length) {
          if (isNumber(text[end])) {
            end++;
            continue;
          } else if (text[end] == ".") {
            if (dotReached) throw ScanException();
            dotReached = true;
            end++;
            continue;
          }
          break;
        }
        String value = text.substring(start, end);
        tokens.add(Token(TokenType.NUMBER, value, value, 0));
        current = end + 1;
    }
  }
  return tokens;
}

abstract class Expression<T> {
  accept(ExpressionVisitor visitor);
}

class BinaryExpression extends Expression {
  final Expression left;
  final Expression right;
  final Token token;

  BinaryExpression(this.left, this.right, this.token);

  @override
  accept(ExpressionVisitor visitor) {
    visitor.visitBinary(this);
  }
}

class LiteralExpression extends Expression {
  final dynamic value;

  LiteralExpression(this.value);

  @override
  accept(ExpressionVisitor visitor) {
    visitor.visitLiteral(this);
  }
}

class GroupingExpression extends Expression {
  final Expression expression;

  GroupingExpression(this.expression);

  @override
  accept(ExpressionVisitor visitor) {
    visitor.visitGrouping(this);
  }
}

class UnaryExpression extends Expression {
  final Expression expression;
  final Token token;

  UnaryExpression(this.expression, this.token);

  @override
  accept(ExpressionVisitor visitor) {
    visitor.visitUnary(this);
  }
}

abstract class ExpressionVisitor {
  String visitUnary(UnaryExpression unaryExpression);

  String visitBinary(BinaryExpression binaryExpression);

  String visitLiteral(LiteralExpression literalExpression);

  String visitGrouping(GroupingExpression groupingExpression);

  String parenthesize(String name, List<Expression> expressions) {
    String builder = "(" + name;
    for (Expression expr in expressions) {
      builder += " ";
      builder += expr.accept(this);
    }
    return builder + ")";
  }
}

class ASTPrinter extends ExpressionVisitor {
  @override
  String visitBinary(BinaryExpression expression) {
    return parenthesize(
        expression.token.lexeme, [expression.left, expression.right]);
  }

  @override
  String visitUnary(UnaryExpression expression) {
    return parenthesize(expression.token.lexeme, [expression.expression]);
  }

  @override
  String visitGrouping(GroupingExpression expression) {
    return parenthesize('group', [expression.expression]);
  }

  @override
  String visitLiteral(LiteralExpression expression) {
    if (expression.value == null) return "nil";
    return expression.value.toString();
  }
}

const TokenType NUMBER = TokenType.NUMBER;
const TokenType FALSE = TokenType.FALSE;
const TokenType TRUE = TokenType.TRUE;
const TokenType NIL = TokenType.NIL;
const TokenType STRING = TokenType.STRING;
const TokenType LEFT_PAREN = TokenType.LEFT_PAREN;
const TokenType RIGHT_PAREN = TokenType.RIGHT_PAREN;
const TokenType SLASH = TokenType.SLASH;
const TokenType STAR = TokenType.STAR;
const TokenType MINUS = TokenType.MINUS;
const TokenType PLUS = TokenType.PLUS;

const TokenType BANG_EQUAL = TokenType.BANG_EQUAL;
const TokenType EQUAL_EQUAL = TokenType.EQUAL_EQUAL;

class Parser {
  final List<Token> tokens;

  Parser(this.tokens);

  int current = 0;

  Token peek() {
    return tokens[current];
  }

  bool isAtEnd() {
    return peek().type == TokenType.EOF;
  }

  Token previous() {
    return tokens[current - 1];
  }

  Token advance() {
    if (!isAtEnd()) current++;
    return previous();
  }

  bool check(TokenType type) {
    if (isAtEnd()) return false;
    return peek().type == type;
  }

  bool match(List<TokenType> types) {
    for (TokenType type in types) {
      if (check(type)) {
        advance();
        return true;
      }
    }
    return false;
  }

  Token consume(TokenType type, String message) {
    if (check(type)) return advance();
    throw Exception("consume error");
  }

  Expression primary() {
    if (match([FALSE])) return LiteralExpression(false);
    if (match([TRUE])) return LiteralExpression(true);
    if (match([NIL])) return LiteralExpression(null);

    if (match([NUMBER, STRING])) {
      return LiteralExpression(previous().literal);
    }

    if (match([LEFT_PAREN])) {
      Expression expr = expression();
      consume(RIGHT_PAREN, "Expect ')' after expression.");
      return GroupingExpression(expr);
    }
    // return end of file
    return LiteralExpression(null);
  }

  Expression unary() {
    if (match([TokenType.BANG, TokenType.MINUS])) {
      Token operator = previous();
      Expression right = unary();
      return UnaryExpression(right, operator);
    }
    return primary();
  }

  Expression factor() {
    Expression expr = unary();

    while (match([SLASH, STAR])) {
      Token operator = previous();
      Expression right = unary();
      expr = BinaryExpression(expr, right, operator);
    }

    return expr;
  }

  Expression term() {
    Expression expr = factor();

    while (match([MINUS, PLUS])) {
      Token operator = previous();
      Expression right = factor();
      expr = BinaryExpression(expr, right, operator);
    }

    return expr;
  }

  Expression comparison() {
    Expression expr = term();

    while (match([
      TokenType.GREATER,
      TokenType.GREATER_EQUAL,
      TokenType.LESS,
      TokenType.LESS_EQUAL
    ])) {
      Token operator = previous();
      Expression right = term();
      expr = BinaryExpression(expr, right, operator);
    }

    return expr;
  }

  Expression equality() {
    Expression expr = comparison();

    while (match([BANG_EQUAL, EQUAL_EQUAL])) {
      Token operator = previous();
      Expression right = comparison();
      expr = BinaryExpression(expr, right, operator);
    }
    return expr;
  }

  Expression expression() {
    return equality();
  }

  Expression parse() {
    return expression();
  }
}
