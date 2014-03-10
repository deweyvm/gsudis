package com.deweyvm.gsudis

abstract class Parser(op:String,
                      numBytes:Int,
                      val req:AltState,
                      val newState:AltState,
                      val printer:OpPrinter) extends OpPrinter with OpParser {
  override def print(parsed:ParsedOp) = printer.print(parsed)
  override val name:String = op
  override val reqState = req
  final def process(state:AltState, input:Vector[Byte]) = {
    if (!(state >= reqState)) {
      None
    } else {
      myProcess(input) flatMap {case (args, rest) =>
        Some((ParsedOp(this, op, args, numBytes), newState, rest))
      }
    }
  }
  def myProcess(input:Vector[Byte]):Option[(Vector[String], Vector[Byte])]
}


//half byte op, half byte arg
class HalfOp(op:String, code:Char, f:Char=>Boolean=(x=>true), reqState:AltState=AltNone, newState:AltState= AltNone)(p:OpPrinter) extends Parser(op, 1, reqState, newState, p) {
  def myProcess(input:Vector[Byte]) = input match {
    case Byte(c, x) +: rest if c == code && f(x) =>
      val v = Vector(x.toString)
      Some((v, rest))
    case _ => None
  }
}

//half byte opcode, half byte register, one byte arg
class RegArgOp(op:String, code:Char, reqState:AltState=AltNone, newState:AltState= AltNone)(p:OpPrinter) extends Parser(op, 2, reqState, newState, p) {
  def myProcess(input:Vector[Byte]) = input match {
    case Byte(c, x) +: b1 +: rest if c == code =>
      val v = Vector(x.toString, b1.toString)
      Some((v, rest))
    case _ => None
  }
}

//half byte opcode, half byte register, two byte arg
class RegArg2Op(op:String, code:Char, reqState:AltState=AltNone, newState:AltState= AltNone)(p:OpPrinter) extends Parser(op, 3, reqState, newState, p) {
  def myProcess(input:Vector[Byte]) = input match {
    case Byte(c, x) +: b1 +: b2 +: rest if c == code =>
      val v = Vector(x.toString, b1.toString, b2.toString)
      Some((v, rest))
    case _ => None
  }
}

//one byte opcode, no args
class WordOp(op:String, code:Byte, reqState:AltState=AltNone, newState:AltState=AltNone)(p:OpPrinter) extends Parser(op, 1, reqState, newState, p) {
  def myProcess(input:Vector[Byte]) = input match {
    case b +: rest if b == code =>
      val v = Vector()
      Some((v, rest))
    case _ => None
  }
}
//one byte opcode, one byte arg
class ArgOp(op:String, code:Byte, reqState:AltState=AltNone, newState:AltState=AltNone)(p:OpPrinter) extends Parser(op, 2, reqState, newState, p) {
  def myProcess(input:Vector[Byte]) = input match {
    case b1 +: b2 +: rest if code == b1 =>
      val v = Vector(b2.toString)
      Some((v, rest))
    case _ => None
  }
}

//move is a special case
class MoveOp(op:String, code1:Char, code2:Char, reqState:AltState=AltNone, newState:AltState=AltNone)(p:OpPrinter) extends Parser(op, 2, reqState, newState, p) {
  def myProcess(input:Vector[Byte]) = input match {
    case Byte(u1, l1) +: Byte(u2, l2) +: rest if code1 == u1 && code2 == u2 =>
      val v = Vector(l1.toString, l2.toString)
      Some((v, rest))
    case _ => None
  }
}


trait OpParser {
  val reqState:AltState
  val name:String
  def process(state:AltState, input:Vector[Byte]):Option[(ParsedOp, AltState, Vector[Byte])]
  override def toString = name
}

