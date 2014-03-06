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

class SingleOp(code:Byte, op:String, override val reqState:AltState, newState:AltState) extends OpParser {
  override val name:String = op
  def process(state:AltState, input:Vector[Byte]) = input match {
    case b +: rest if b == code && state >= reqState =>
      Some((ParsedOp(op, Vector()), newState, rest))
    case _ => None
  }
}

class ArgOp(code:Byte, op:String, override val reqState:AltState, newState:AltState) extends OpParser {
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

  //put alt3 ops first, then alt2, then alt1, tried in order
  //case object Iwt extends Op
  val ToParser     = new HalfOp('1', "to", AltNone, AltNone) with RegisterPrinter
  val WithParser   = new HalfOp('2', "with", AltNone, AltNone) with RegisterPrinter
  val AddParser = new HalfOp('5', "add", AltNone, AltNone) with RegisterPrinter
  val AddiParser = new HalfOp('5', "addi", AltState2, AltNone) with ImmediatePrinter
  val AdcParser = new HalfOp('5', "adc", AltState1, AltNone) with RegisterPrinter
  val AdciParser = new HalfOp('5', "adci", AltState3, AltNone) with ImmediatePrinter


  //these parsers must be last
  val Alt1Parser   = new SingleOp("3D".b, "alt1", AltNone, AltState1) with OpNamePrinter
  val Alt2Parser   = new SingleOp("3E".b, "alt2", AltNone, AltState2) with OpNamePrinter
  val Alt3Parser   = new SingleOp("3F".b, "alt3", AltNone, AltState3) with OpNamePrinter

  val AndParser  = new HalfOp('7', "and", AltNone, AltNone) with RegisterPrinter
  val AndiParser = new HalfOp('7', "andi", AltState2, AltNone) with ImmediatePrinter
  val AsrParser = new SingleOp("96".b, "asr", AltNone, AltNone) with OpNamePrinter
  val BccParser = new ArgOp("0C".b, "bcc", AltNone, AltNone) with AddressPrinter
  val BcsParser = new ArgOp("0D".b, "bcs", AltNone, AltNone) with AddressPrinter
  val BeqParser = new ArgOp("09".b, "beq", AltNone, AltNone) with AddressPrinter
  val BgeParser = new ArgOp("07".b, "bge", AltNone, AltNone) with AddressPrinter
  val BicParser = new HalfOp('7', "bic", AltState1, AltNone) with RegisterPrinter
  val BiciParser = new HalfOp('7', "bici", AltState3, AltNone) with RegisterPrinter
  val BltParser = new ArgOp("06".b, "blt", AltNone, AltNone) with AddressPrinter
  val BmiParser = new ArgOp("0B".b, "bmi", AltNone, AltNone) with AddressPrinter
  val BneParser = new ArgOp("08".b, "bne", AltNone, AltNone) with AddressPrinter
  val BraParser = new ArgOp("05".b, "bra", AltNone, AltNone) with AddressPrinter
  val BvcParser = new ArgOp("0E".b, "bvc", AltNone, AltNone) with AddressPrinter
  val BvsParser = new ArgOp("0E".b, "bvs", AltNone, AltNone) with AddressPrinter

  val CacheParser = new SingleOp("02".b, "cache", AltNone, AltNone) with OpNamePrinter
  val CModeParser = new SingleOp("4E".b, "cmode", AltState1, AltNone) with OpNamePrinter
  val CmpParser = new HalfOp('6', "cmp", AltState3, AltNone) with RegisterPrinter
  val ColorParser = new SingleOp("4E".b, "color", AltNone, AltNone) with OpNamePrinter
  val DecParser = new HalfOp('E', "dec", AltNone, AltNone) with RegisterPrinter
  val Div2Parser = new SingleOp("96".b, "div2", AltState1, AltNone) with OpNamePrinter
  val FMultParser = new SingleOp("9F".b, "fmult", AltNone, AltNone) with OpNamePrinter
  val FromParser = new HalfOp('B', "from", AltNone, AltNone) with RegisterPrinter
  val GetBParser = new SingleOp("EF".b, "getb", AltNone, AltNone) with OpNamePrinter
  val GetBHParser = new SingleOp("EF".b, "getbh", AltState1, AltNone) with OpNamePrinter




  val NopParser = new SingleOp("01".b, "nop", AltNone, AltNone) with OpNamePrinter

  val All =
    Vector(
      GetBHParser,
      GetBParser,
    FMultParser,
    Div2Parser,
    DecParser,
    ColorParser,
    CmpParser,
    CModeParser,
    CacheParser,
    BvsParser, BraParser, BvcParser, BneParser, BmiParser, BltParser,
    BiciParser,
    BicParser,
    BgeParser,
    BeqParser,
    BcsParser,
    BccParser,
    AsrParser,
    AndiParser, AndParser,
    AdciParser, AddiParser, AdcParser, AddParser,
    FromParser,
    ToParser,
    WithParser,
    NopParser,
    Alt1Parser, Alt2Parser, Alt3Parser).sortWith { case (o1, o2) =>
       o1.reqState.value > o1.reqState.value
    }
}

object Main {
  def main(args:Array[String]) {
    import Op._
    val hex = "3D EF 9F EF E1 E2 4E 3F 61 3D 4E 02 02 0E 07 05 06 08 08 0B 23 06 22 3D 71 3F 71 0D FF 09 FF 07 FF 0C FF 71 3E 71 3F 55 3D 51 3E 5F B2 15 51 4E 3D 96".split(" ")
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
