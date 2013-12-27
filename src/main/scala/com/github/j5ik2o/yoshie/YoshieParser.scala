package com.github.j5ik2o.yoshie

import scala.util.parsing.combinator.RegexParsers
import scala.collection.mutable.Map

object Main {

  def main(args: Array[String]): Unit = {
    val expr = """
def twice(x) = x * 2;
println("twice(3):" + twice(3));
def rec(x) = {
  if(0 < x){
    println(x);
    rec(x - 1)
  }else{
    0
  }
};
rec(3);
val tw = twice;
println("tw(5):" + tw(5));
               """
    val parser = new MiniParser
    val ast = parser.parse(expr).get

    val visitor = new ExprVisitor
    var result = visitor.eval(new Environment(None), ast);
  }
}

class Environment(parent: Option[Environment]) {
  val variables = Map[String, Any]()

  def get(key: String): Any = {
    if (variables.contains(key)) {
      variables(key)
    } else {
      parent match {
        case Some(p) => p.get(key)
        case None => throw new Exception("symbol'%s' not found".format(key))
      }
    }

  }

  def set(key: String, value: Any) {
    variables(key) = value
  }
}


class ExprVisitor {
  def eval(env: Environment, ast: AST): Any = {
    def visit(ast: AST): Any = {
      ast match {
        case Lines(exprs) =>
          val local = new Environment(Some(env))
          exprs.foldLeft(Unit: Any) {
            (result, x) => eval(local, x)
          }
        case IfExpr(cond, pos, neg) =>
          visit(cond) match {
            case true => visit(pos)
            case false => visit(neg)
          }
        case LessOp(left, right) =>
          (visit(left), visit(right)) match {
            case (l: Int, r: Int) => l < r
          }
        case AddOp(left, right) =>
          (visit(left), visit(right)) match {
            case (l: Int, r: Int) => l + r
            case (l: String, r) => l + r
            case (l, r: String) => l + r
          }
        case SubOp(left, right) =>
          (visit(left), visit(right)) match {
            case (l: Int, r: Int) => l - r
          }
        case MulOp(left, right) =>
          (visit(left), visit(right)) match {
            case (l: Int, r: Int) => l * r
          }
        case IntVal(value) => value
        case StringVal(value) => value
        case PrintLine(value) =>
          val v = visit(value)
          println(v)
          v
        case Ident(name) =>
          env.get(name)
        case Assignment(vr, value) =>
          val v = visit(value)
          env.set(vr, v)
        case FuncDef(name, func) =>
          env.set(name, func)
        case FuncCall(func, params) =>
          visit(func) match {
            case f: Func =>
              val local = new Environment(Some(env))
              var arg = params
              for (pn <- f.params) {
                val a :: suc = arg
                local.set(pn, visit(a))
                arg = suc
              }
              eval(local, f.proc)
          }
      }
    }
    visit(ast)
  }
}

trait AST

case class Lines(exprs: List[AST]) extends AST

case class IfExpr(cond: AST, pos: AST, neg: AST) extends AST

case class LessOp(left: AST, right: AST) extends AST

case class AddOp(left: AST, right: AST) extends AST

case class SubOp(left: AST, right: AST) extends AST

case class MulOp(left: AST, right: AST) extends AST

case class StringVal(value: String) extends AST

case class PrintLine(value: AST) extends AST

case class IntVal(value: Int) extends AST

case class Ident(name: String) extends AST

case class Assignment(variable: String, value: AST) extends AST

case class Func(params: List[String], proc: AST) extends AST

case class FuncDef(name: String, func: Func) extends AST

case class FuncCall(func: AST, params: List[AST]) extends AST

class MiniParser extends RegexParsers {

  //lines ::= expr {";" expr} [";"]
  def lines: Parser[AST] = repsep(line, ";") <~ opt(";") ^^ Lines

  def line: Parser[AST] = expr | assignment | funcDef

  //expr ::= cond | if | printLine
  def expr: Parser[AST] = condOp | ifExpr | printLine

  //if ::= "if" "(" expr ")" expr "else" expr
  def ifExpr: Parser[AST] = "if" ~ "(" ~> expr ~ ")" ~ expr ~ "else" ~ expr ^^ {
    case cond ~ _ ~ pos ~ _ ~ neg => IfExpr(cond, pos, neg)
  }

  //cond ::= add {"<" add}
  def condOp: Parser[AST] = chainl1(add,
    "<" ^^ {
      op => (left: AST, right: AST) => LessOp(left, right)
    })

  //add ::= term {"+" term | "-" term}.
  def add: Parser[AST] = chainl1(term,
    "+" ^^ {
      op => (left: AST, right: AST) => AddOp(left, right)
    } |
      "-" ^^ {
        op => (left: AST, right: AST) => SubOp(left, right)
      })

  //term ::= factor {"*" factor}
  def term: Parser[AST] = chainl1(funcCall,
    "*" ^^ {
      op => (left: AST, right: AST) => MulOp(left, right)
    })

  def funcCall: Parser[AST] = factor ~ opt("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case fac ~ param =>
      param match {
        case Some(p) => FuncCall(fac, p)
        case None => fac
      }
  }

  //factor ::= intLiteral | stringLiteral | "(" expr ")" | "{" lines "}"
  def factor: Parser[AST] = intLiteral | stringLiteral | ident |
    "(" ~> expr <~ ")" | "{" ~> lines <~ "}"

  //intLiteral ::= ["1"-"9"] {"0"-"9"}
  def intLiteral: Parser[AST] = """[1-9][0-9]*|0""".r ^^ {
    value => IntVal(value.toInt)
  }

  //stringLiteral ::= "\"" {"a-zA-Z0-9.."}  "\""
  def stringLiteral: Parser[AST] = "\"" ~> """[a-zA-Z0-9:*/+\- ()]*""".r <~ "\"" ^^ StringVal

  def ident: Parser[Ident] = """[A-Za-z_][a-zA-Z0-9]*""".r ^? {
    case n if n != "if" && n != "val" && n != "println" && n != "def" => n
  } ^^ Ident

  def assignment: Parser[Assignment] = "val" ~> ident ~ "=" ~ expr ^^ {
    case v ~ _ ~ value => Assignment(v.name, value)
  }

  // printLine ::= "printLn" "(" expr ")"
  def printLine: Parser[AST] = "println" ~ "(" ~> expr <~ ")" ^^ PrintLine

  def funcDef: Parser[FuncDef] = "def" ~> ident ~ opt("(" ~> repsep(ident, ",") <~ ")") ~ "=" ~ expr ^^ {
    case v ~ params ~ _ ~ proc => {
      val p = params match {
        case Some(pr) => pr
        case None => Nil
      }
      FuncDef(v.name, Func(for (pm <- p) yield {
        pm.name
      }, proc))
    }
  }

  def parse(str: String) = parseAll(lines, str)
}
