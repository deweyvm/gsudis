package com.deweyvm.gsudis

//fixme, AltState2 !> AltState1
abstract class AltState(val value:Int) {
  def >=(other:AltState):Boolean
}
case object AltNone extends AltState(0) {
  override def >=(other:AltState) = other.value == 0
}
case object AltState1 extends AltState(1) {
  override def >=(other:AltState) = other.value <= 1
}
case object AltState2 extends AltState(2) {
  override def >=(other:AltState) = other.value == 2 || other.value == 0
}
case object AltState3 extends AltState(3) {
  override def >=(other:AltState) = true
}

case class Byte(upper:Char, lower:Char) {
  override def toString = "%s%s" format (upper, lower)
}


trait OpPrinter {
  def print(parsed:ParsedOp):String
  def reg(s:String) = "r" + Integer.parseInt(s, 16)
  def imm(s:String) = "#$" + s
  def adr(s:String) = "$" + s
}

case class Printer(v:Vector[String=>String]) extends OpPrinter {
  def this() = this(Vector())
  def +(f:String=>String) = Printer(f +: v)
  def print(parsed:ParsedOp):String = parsed.op + " " + parsed.args.zip(v).map {case (p, f) => f(p)}.mkString(", ")
}


trait RegisterPrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " " + reg(parsed.args(0))
}

trait ImmediatePrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " " + imm(parsed.args(0))
}

trait OpNamePrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op
}

trait AddressPrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " " + adr(parsed.args(0))
}

trait RegisterImmediatePrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " " + reg(parsed.args(0)) + ", " + imm(parsed.args(1))
}

trait RegisterAddressPrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " " + reg(parsed.args(0)) + ", " + adr(parsed.args(1))
}


trait OpParser {
  val reqState:AltState
  val name:String
  def process(state:AltState, input:Vector[Byte]):Option[(ParsedOp, AltState, Vector[Byte])]
  override def toString = name
}

case class ParsedOp(op:String, args:Vector[String])
object HalfOp {
  def fTrue(c:Char) = true
}
//half byte op, half byte arg
class HalfOp(val op:String, code:Char, f:Char=>Boolean=HalfOp.fTrue, override val reqState:AltState=AltNone, newState:AltState= AltNone) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case Byte(c, x) +: rest if c == code && state >= reqState && f(x) =>
      Some((ParsedOp(op, Vector(x.toString)), newState, rest))
    case _ => None
  }
}

//half byte opcode, half byte register, one byte arg
class RegArgOp(val op:String, code:Char, override val reqState:AltState=AltNone, newState:AltState= AltNone) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case Byte(c, x) +: b1 +: rest if c == code && state >= reqState =>
      Some((ParsedOp(op, Vector(x.toString, b1.toString)), newState, rest))
    case _ => None
  }
}

//half byte opcode, half byte register, one byte arg
class RegArg2Op(val op:String, code:Char, override val reqState:AltState=AltNone, newState:AltState= AltNone) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case Byte(c, x) +: b1 +: b2 +: rest if c == code && state >= reqState =>
      Some((ParsedOp(op, Vector(x.toString, b1.toString, b2.toString)), newState, rest))
    case _ => None
  }
}

//one byte opcode, no args
class WordOp(val op:String, code:Byte, override val reqState:AltState=AltNone, newState:AltState=AltNone) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case b +: rest if b == code && state >= reqState =>
      Some((ParsedOp(op, Vector()), newState, rest))
    case _ => None
  }
}
//one byte opcode, one byte arg
class ArgOp(val op:String, code:Byte, override val reqState:AltState=AltNone, newState:AltState=AltNone) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case b1 +: b2 +: rest if code == b1 && state >= reqState =>
      Some((ParsedOp(op, Vector(b2.toString)), newState, rest))
    case _ => None
  }
}




object Op {
  implicit class string2Byte(s:String) {
    def b:Byte = {
      val upper = s(0)
      val lower = s(1)
      Byte(upper, lower)
    }
  }
  val All:Vector[OpParser with OpPrinter] = Vector(
    new HalfOp("add",  '5') with RegisterPrinter,
    new HalfOp("addi", '5', reqState=AltState2) with ImmediatePrinter,
    new HalfOp("adc",  '5', reqState=AltState1) with RegisterPrinter,
    new HalfOp("adci", '5', reqState=AltState3) with ImmediatePrinter,
    new WordOp("alt1", "3D".b, newState=AltState1) with OpNamePrinter,
    new WordOp("alt2", "3E".b, newState=AltState2) with OpNamePrinter,
    new WordOp("alt3", "3F".b, newState=AltState3) with OpNamePrinter,
    new HalfOp("and", '7', _ >= '1') with RegisterPrinter,
    new HalfOp("andi",'7', _ >= '1', reqState=AltState2) with ImmediatePrinter,
    new WordOp("asr", "96".b) with OpNamePrinter,
    new ArgOp("bcc", "0C".b) with AddressPrinter,
    new ArgOp("bcs", "0D".b) with AddressPrinter,
    new ArgOp("beq", "09".b) with AddressPrinter,
    new ArgOp("bge", "07".b) with AddressPrinter,
    new HalfOp("bic", '7', _ >= '1', reqState=AltState1) with RegisterPrinter,
    new HalfOp("bici",'7', _ >= '1', reqState=AltState3) with ImmediatePrinter,
    new ArgOp("blt", "06".b) with AddressPrinter,
    new ArgOp("bmi", "0B".b) with AddressPrinter,
    new ArgOp("bne", "08".b) with AddressPrinter,
    new ArgOp("bra", "05".b) with AddressPrinter,
    new ArgOp("bvc", "0E".b) with AddressPrinter,
    new ArgOp("bvs", "0E".b) with AddressPrinter,
    new WordOp("cache", "02".b) with OpNamePrinter,
    new WordOp("cmode", "4E".b, reqState=AltState1) with OpNamePrinter,
    new HalfOp("cmp", '6', reqState=AltState3) with RegisterPrinter,
    new WordOp("color", "4E".b) with OpNamePrinter,
    new HalfOp("dec", 'E', _ <= 'E') with RegisterPrinter,
    new WordOp("div2", "96".b, reqState=AltState1) with OpNamePrinter,
    new WordOp("fmult","9F".b) with OpNamePrinter,
    new HalfOp("from", 'B') with RegisterPrinter,
    new WordOp("getb", "EF".b) with OpNamePrinter,
    new WordOp("getbh", "EF".b, reqState=AltState1) with OpNamePrinter,
    new WordOp("getbl", "EF".b, reqState=AltState2) with OpNamePrinter,
    new WordOp("getbs", "EF".b, reqState=AltState3) with OpNamePrinter,
    new WordOp("getc", "DF".b) with OpNamePrinter,
    new WordOp("hib", "C0".b) with OpNamePrinter,
    new RegArgOp("itb", 'A') with RegisterImmediatePrinter,
    new HalfOp("inc", 'D', _ <= 'E') with RegisterPrinter,
    //iwt  4 reg imm2
    new HalfOp("jmp", '9', _ >= '8') with RegisterPrinter,
    new HalfOp("ldb", '4', _ <= 'B', reqState=AltState1) with RegisterPrinter,
    new HalfOp("ldw", '4', _ <= 'B') with RegisterPrinter,
    //lea  4 reg imm2
    new HalfOp("link", '9', x => x >= '1' && x <= '4') with ImmediatePrinter,
    new HalfOp("ljmp", '9', x => x >= '8' && x <= 'D') with RegisterPrinter,
    //lm   4 reg imm2
    new RegArgOp("lms", 'A', reqState=AltState1) with RegisterImmediatePrinter,//lms  4 reg imm
    new WordOp("lmult", "9F".b, reqState=AltState1) with OpNamePrinter,
    new WordOp("lob", "9E".b) with OpNamePrinter,
    new WordOp("loop", "3C".b) with OpNamePrinter,
    new WordOp("lsr", "03".b) with OpNamePrinter,
    new WordOp("merge", "70".b) with OpNamePrinter,
    //move
    //ALL THE MOVES
    new HalfOp("mult", '8') with RegisterPrinter,
    new HalfOp("multi", '8', reqState=AltState2) with ImmediatePrinter,
    new WordOp("nop", "01".b) with OpNamePrinter,
    new WordOp("not", "4F".b) with OpNamePrinter,
    new HalfOp("or", 'C', _ >= '1') with RegisterPrinter,
    new HalfOp("ori", 'C',_ >= '1',  reqState=AltState2) with ImmediatePrinter,
    new WordOp("plot", "4C".b) with OpNamePrinter,
    new WordOp("ramb", "4C".b, reqState=AltState2) with OpNamePrinter,
    new WordOp("rol", "04".b) with OpNamePrinter,
    new WordOp("romb", "DF".b, reqState=AltState3) with OpNamePrinter,
    new WordOp("ror", "97".b) with OpNamePrinter,
    new WordOp("rpix", "4C".b, reqState=AltState1) with OpNamePrinter,
    new HalfOp("sbc", '6', reqState=AltState1) with RegisterPrinter,
    new WordOp("sbk", "90".b) with OpNamePrinter,
    new WordOp("sex", "95".b) with OpNamePrinter,
    //sm  4 reg imm2
    new RegArgOp("sms", 'A', reqState=AltState2) with RegisterAddressPrinter,//sms 4 reg imm
    new HalfOp("stb", '3', _ <= 'B') with RegisterPrinter,
    new WordOp("stop", "00".b) with OpNamePrinter,
    new HalfOp("stw", '3', _ <= 'B') with RegisterPrinter,
    new HalfOp("sub", '6') with RegisterPrinter,
    new HalfOp("subi", '6', reqState=AltState2) with ImmediatePrinter,
    new WordOp("swap", "4D".b) with OpNamePrinter,
    new HalfOp("to", '1') with RegisterPrinter,
    new HalfOp("umul", '8', reqState=AltState1) with RegisterPrinter,

    new HalfOp("with", '2') with RegisterPrinter


  ).sortWith { case (o1, o2) =>
       o1.reqState.value > o2.reqState.value
  }
}

object Main {
  def main(args:Array[String]) {
    import Op._
    val hex = "3D 61 3D 81 4D 3E 61 61 3B 00 95 3D 4C 90 3F DF 3E 4C 4C 3E C2 C2 4F 3E 81 01 85 03 3C 9E 3D 9F 45 D5 C0 DF EF 3D EF 3F EF 3E EF 3D 51 3D EF 9F EF E1 E2 4E 3F 61 3D 4E 02 02 0E 07 05 06 08 08 0B 23 06 22 3D 71 3F 71 0D FF 09 FF 07 FF 0C FF 71 3E 71 3F 55 3D 51 3E 5F B2 15 51 4E 3D 96 04 97 98 4B 4C 91 92 93 94 98 99 9A 9B 9C 9D 3D 3B A2 12 3D A2 15 3E A5 FF".split(" ")
    val a = hex.map {_.b}.toVector
    var state:AltState = AltNone
    var rest = a
    while (rest.length > 0) {
      import scala.util.control.Breaks._
      breakable {
        val prevLength = rest.length
        for (op <- Op.All) {
          op.process(state, rest) match {
            case Some((parsed, newState, newRest)) =>
              println(op.print(parsed))
              state = newState
              rest = newRest
              break()
            case _ =>
              ()
          }
        }
        if (rest.length == prevLength) {
          throw new Exception("Failed to find a parser at " + rest)
        }
      }
    }
  }
}
