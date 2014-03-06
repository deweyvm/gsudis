package com.deweyvm.gsudis

//fixme, AltState2 !> AltState1
class AltState(val value:Int) {
  def >=(other:AltState) = value >= other.value
}
case object AltNone extends AltState(0)
case object AltState1 extends AltState(1)
case object AltState2 extends AltState(2)
case object AltState3 extends AltState(3)

case class Byte(upper:Char, lower:Char) {
  override def toString = "%s%s" format (upper, lower)
}

trait OpPrinter {
  def print(parsed:ParsedOp):String
}

trait RegisterPrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " r" + parsed.args(0)
}

trait ImmediatePrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " #$" + parsed.args(0)
}

trait OpNamePrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op
}

trait AddressPrinter extends OpPrinter {
  def print(parsed:ParsedOp):String = parsed.op + " $" + parsed.args(0)
}

trait OpParser {
  val reqState:AltState
  val name:String
  def process(state:AltState, input:Vector[Byte]):Option[(ParsedOp, AltState, Vector[Byte])]
  override def toString = name
}

case class ParsedOp(op:String, args:Vector[String])

class HalfOp(code:Char, val op:String, override val reqState:AltState, newState:AltState) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case Byte(c, x) +: rest if c == code && state >= reqState =>
      Some((ParsedOp(op, Vector(x.toString)), newState, rest))
    case _ => None
  }
}

class SingleOp(code:Byte, val op:String, override val reqState:AltState, newState:AltState) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case b +: rest if b == code && state >= reqState =>
      Some((ParsedOp(op, Vector()), newState, rest))
    case _ => None
  }
}

class ArgOp(code:Byte, val op:String, override val reqState:AltState, newState:AltState) extends OpParser {
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
    new HalfOp('5', "add", AltNone, AltNone) with RegisterPrinter,
    new HalfOp('5', "addi", AltState2, AltNone) with ImmediatePrinter,
    new HalfOp('5', "adc", AltState1, AltNone) with RegisterPrinter,
    new HalfOp('5', "adci", AltState3, AltNone) with ImmediatePrinter,
    new SingleOp("3D".b, "alt1", AltNone, AltState1) with OpNamePrinter,
    new SingleOp("3E".b, "alt2", AltNone, AltState2) with OpNamePrinter,
    new SingleOp("3F".b, "alt3", AltNone, AltState3) with OpNamePrinter,
    new HalfOp('7', "and", AltNone, AltNone) with RegisterPrinter,
    new HalfOp('7', "andi", AltState2, AltNone) with ImmediatePrinter,
    new SingleOp("96".b, "asr", AltNone, AltNone) with OpNamePrinter,
    new ArgOp("0C".b, "bcc", AltNone, AltNone) with AddressPrinter,
    new ArgOp("0D".b, "bcs", AltNone, AltNone) with AddressPrinter,
    new ArgOp("09".b, "beq", AltNone, AltNone) with AddressPrinter,
    new ArgOp("07".b, "bge", AltNone, AltNone) with AddressPrinter,
    new HalfOp('7', "bic", AltState1, AltNone) with RegisterPrinter,
    new HalfOp('7', "bici", AltState3, AltNone) with RegisterPrinter,
    new ArgOp("06".b, "blt", AltNone, AltNone) with AddressPrinter,
    new ArgOp("0B".b, "bmi", AltNone, AltNone) with AddressPrinter,
    new ArgOp("08".b, "bne", AltNone, AltNone) with AddressPrinter,
    new ArgOp("05".b, "bra", AltNone, AltNone) with AddressPrinter,
    new ArgOp("0E".b, "bvc", AltNone, AltNone) with AddressPrinter,
    new ArgOp("0E".b, "bvs", AltNone, AltNone) with AddressPrinter,
    new SingleOp("02".b, "cache", AltNone, AltNone) with OpNamePrinter,
    new SingleOp("4E".b, "cmode", AltState1, AltNone) with OpNamePrinter,
    new HalfOp('6', "cmp", AltState3, AltNone) with RegisterPrinter,
    new SingleOp("4E".b, "color", AltNone, AltNone) with OpNamePrinter,
    new HalfOp('E', "dec", AltNone, AltNone) with RegisterPrinter,
    new SingleOp("96".b, "div2", AltState1, AltNone) with OpNamePrinter,
    new SingleOp("9F".b, "fmult", AltNone, AltNone) with OpNamePrinter,
    new HalfOp('B', "from", AltNone, AltNone) with RegisterPrinter,
    new SingleOp("EF".b, "getb", AltNone, AltNone) with OpNamePrinter,
    new SingleOp("EF".b, "getbh", AltState1, AltNone) with OpNamePrinter,
    
    new HalfOp('1', "to", AltNone, AltNone) with RegisterPrinter,
    new HalfOp('2', "with", AltNone, AltNone) with RegisterPrinter,

    new SingleOp("01".b, "nop", AltNone, AltNone) with OpNamePrinter

  ).sortWith { case (o1, o2) =>
       o1.reqState.value > o2.reqState.value
  }
}

object Main {
  def main(args:Array[String]) {
    import Op._
    val hex = "3D 51 3D EF 9F EF E1 E2 4E 3F 61 3D 4E 02 02 0E 07 05 06 08 08 0B 23 06 22 3D 71 3F 71 0D FF 09 FF 07 FF 0C FF 71 3E 71 3F 55 3D 51 3E 5F B2 15 51 4E 3D 96".split(" ")
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
