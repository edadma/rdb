package io.github.edadma.rdb

def decorate(expr: Expr): Unit =
  expr match
    case ApplyExpr(name, args) =>
    case BinaryExpr(left, op, right) =>
    case OperatorExpr(oper) =>
      case