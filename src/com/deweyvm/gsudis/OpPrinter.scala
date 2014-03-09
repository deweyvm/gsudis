package com.deweyvm.gsudis

trait OpPrinter {
  def print(parsed:ParseResult):String
}

object OpPrinter {
  def padLeft(n:Int, s:String) = ("0"*n).substring(s.length) + s

  def reg(s:String) = "r" + Integer.parseInt(s, 16)
  def imm(s:String) = "#$" + padLeft(2, s)
  def imm4(a:String, b:String) = "#$"  + padLeft(2, a) + padLeft(2, b)
  def adr(s:String) = "$" + padLeft(2, s)
  def paren(s:String) = "(" + s + ")"
}

object Printer {
  def apply() = new Printer(Vector())
}
class Printer(v:Vector[String=>String]) extends OpPrinter {
  def +(f:String=>String) = new Printer(v :+ f)
  def print(parsed:ParseResult):String = parsed.op + " " + parsed.args.zip(v).map {case (p, f) => f(p)}.mkString(",")
}

