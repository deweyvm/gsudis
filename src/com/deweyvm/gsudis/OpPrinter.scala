package com.deweyvm.gsudis

trait OpPrinter {
  def print(parsed:ParsedOp):String
}

object OpPrinter {
  def reg(s:String) = "r" + Integer.parseInt(s, 16)
  def imm(s:String) = "#$" + s
  def adr(s:String) = "$" + s
}

object Printer {
  def apply() = new Printer(Vector())
}
class Printer(v:Vector[String=>String]) extends OpPrinter {
  def +(f:String=>String) = new Printer(v :+ f)
  def print(parsed:ParsedOp):String = parsed.op + " " + parsed.args.zip(v).map {case (p, f) => f(p)}.mkString(", ")
}


trait OpParser extends OpPrinter {
  val reqState:AltState
  val name:String
  def process(state:AltState, input:Vector[Byte]):Option[(ParsedOp, AltState, Vector[Byte])]
  override def toString = name
}
