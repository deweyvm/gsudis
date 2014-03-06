package com.deweyvm.gsudis

case class ParseResult(printer:OpPrinter, op:String, args:Vector[String]) {
  override def toString = printer.print(this)
}
