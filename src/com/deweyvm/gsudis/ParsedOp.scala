package com.deweyvm.gsudis

case class ParsedOp(printer:OpPrinter, op:String, args:Vector[String], numBytes:Int) {
  override def toString = printer.print(this)
}
