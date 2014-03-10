package com.deweyvm.gsudis

import scala.collection.mutable.ArrayBuffer

object Parsing {
  implicit class string2Byte(s:String) {
    def b:Byte = {
      val upper = s(0)
      val lower = s(1)
      Byte(upper, lower)
    }
  }

  def to2comp(p:Int) = {
    //~p + 1
    p.toByte
  }

  val immPrinter = Printer() + OpPrinter.imm
  val regPrinter = Printer() + OpPrinter.reg
  val wordPrinter = Printer()
  val adrPrinter = Printer() + OpPrinter.adr
  val ramPrinter = Printer() + (OpPrinter.paren _).compose(OpPrinter.reg)
  val jumpPrinter = Printer() + {s => to2comp(Integer.parseInt(s, 16)).toString }
  val loadPrinter = Printer() + OpPrinter.reg + OpPrinter.paren
  val load2Printer = new OpPrinter {
    override def print(parsed: ParsedOp): String = parsed.op + " " + OpPrinter.reg(parsed.args(0)) + "," + OpPrinter.paren(parsed.args(2) + parsed.args(1))
  }
  val regImmPrinter = Printer() + OpPrinter.reg + OpPrinter.imm
  val regImm2Printer = new OpPrinter {
    override def print(parsed: ParsedOp): String = parsed.op + " " + OpPrinter.reg(parsed.args(0)) + "," + OpPrinter.imm4(parsed.args(2), parsed.args(1))
  }
  val smsPrinter = new OpPrinter {
    override def print(parsed: ParsedOp): String = parsed.op + " " + OpPrinter.paren(parsed.args(1)) + "," + OpPrinter.reg(parsed.args(0))
  }
  val smPrinter = new OpPrinter {
    override def print(parsed: ParsedOp): String = parsed.op + " " + OpPrinter.paren(parsed.args(2) + parsed.args(1)) + "," + OpPrinter.reg(parsed.args(0))
  }
  val movePrinter = Printer() + OpPrinter.reg + OpPrinter.reg
  val All:Vector[OpParser with OpPrinter] = Vector(
    new HalfOp("add", '5')(regPrinter),
    new HalfOp("add", '5', reqState=AltState2)(immPrinter),
    new HalfOp("adc", '5', reqState=AltState1)(regPrinter),
    new HalfOp("adc", '5', reqState=AltState3)(immPrinter),
    new WordOp("alt1", "3D".b, newState=AltState1)(wordPrinter),
    new WordOp("alt2", "3E".b, newState=AltState2)(wordPrinter),
    new WordOp("alt3", "3F".b, newState=AltState3)(wordPrinter),
    new HalfOp("and", '7', _ >= '1')(regPrinter),
    new HalfOp("and",'7', _ >= '1', reqState=AltState2)(immPrinter),
    new WordOp("asr", "96".b)(wordPrinter),
    new ArgOp("bcc", "0C".b)(jumpPrinter),
    new ArgOp("bcs", "0D".b)(jumpPrinter),
    new ArgOp("beq", "09".b)(jumpPrinter),
    new ArgOp("bge", "07".b)(jumpPrinter),
    new HalfOp("bic", '7', _ >= '1', reqState=AltState1)(regPrinter),
    new HalfOp("bic",'7', _ >= '1', reqState=AltState3)(immPrinter),
    new ArgOp("blt", "06".b)(jumpPrinter),
    new ArgOp("bmi", "0B".b)(jumpPrinter),
    new ArgOp("bne", "08".b)(jumpPrinter),
    new ArgOp("bpl", "0A".b)(jumpPrinter),
    new ArgOp("bra", "05".b)(jumpPrinter),
    new ArgOp("bvc", "0E".b)(jumpPrinter),
    new ArgOp("bvs", "0F".b)(jumpPrinter),
    new WordOp("cache", "02".b)(wordPrinter),
    new WordOp("cmode", "4E".b, reqState=AltState1)(wordPrinter),
    new HalfOp("cmp", '6', reqState=AltState3)(regPrinter),
    new WordOp("color", "4E".b)(wordPrinter),
    new HalfOp("dec", 'E', _ <= 'E')(regPrinter),
    new WordOp("div2", "96".b, reqState=AltState1)(wordPrinter),
    new WordOp("fmult","9F".b)(wordPrinter),
    new HalfOp("from", 'B')(regPrinter),
    new WordOp("getb", "EF".b)(wordPrinter),
    new WordOp("getbh", "EF".b, reqState=AltState1)(wordPrinter),
    new WordOp("getbl", "EF".b, reqState=AltState2)(wordPrinter),
    new WordOp("getbs", "EF".b, reqState=AltState3)(wordPrinter),
    new WordOp("getc", "DF".b)(wordPrinter),
    new WordOp("hib", "C0".b)(wordPrinter),
    new RegArgOp("ibt", 'A')(regImmPrinter),
    new HalfOp("inc", 'D', _ <= 'E')(regPrinter),
    new RegArg2Op("iwt", 'F')(regImm2Printer),
    new HalfOp("jmp", '9', x => x >= '8' && x <= 'D')(regPrinter),
    new HalfOp("ldb", '4', _ <= 'B', reqState=AltState1)(ramPrinter),
    new HalfOp("ldw", '4', _ <= 'B')(ramPrinter),
    //same as iwt??//new RegArg2Op("lea", 'F')(regImm2Printer),
    new HalfOp("link", '9', x => x >= '1' && x <= '4')(immPrinter),
    new HalfOp("ljmp", '9', x => x >= '8' && x <= 'D', reqState=AltState1)(regPrinter),
    new RegArg2Op("lm", 'F', reqState=AltState1)(load2Printer),
    new RegArgOp("lms", 'A', reqState=AltState1)(loadPrinter),
    new WordOp("lmult", "9F".b, reqState=AltState1)(wordPrinter),
    new WordOp("lob", "9E".b)(wordPrinter),
    new WordOp("loop", "3C".b)(wordPrinter),
    new WordOp("lsr", "03".b)(wordPrinter),
    new WordOp("merge", "70".b)(wordPrinter),
    new MoveOp("move", '2', '1')(movePrinter),
    new MoveOp("moves", '2', 'B')(movePrinter),
    new HalfOp("mult", '8')(regPrinter),
    new HalfOp("mult", '8', reqState=AltState2)(immPrinter),
    new WordOp("nop", "01".b)(wordPrinter),
    new WordOp("not", "4F".b)(wordPrinter),
    new HalfOp("or", 'C', _ >= '1')(regPrinter),
    new HalfOp("or", 'C',_ >= '1',  reqState=AltState2)(immPrinter),
    new WordOp("plot", "4C".b)(wordPrinter),
    new WordOp("ramb", "DF".b, reqState=AltState2)(wordPrinter),
    new WordOp("rol", "04".b)(wordPrinter),
    new WordOp("romb", "DF".b, reqState=AltState3)(wordPrinter),
    new WordOp("ror", "97".b)(wordPrinter),
    new WordOp("rpix", "4C".b, reqState=AltState1)(wordPrinter),
    new HalfOp("sbc", '6', reqState=AltState1)(regPrinter),
    new WordOp("sbk", "90".b)(wordPrinter),
    new WordOp("sex", "95".b)(wordPrinter),
    new RegArg2Op("sm", 'F', reqState=AltState2)(smPrinter),
    new RegArgOp("sms", 'A', reqState=AltState2)(smsPrinter),
    new HalfOp("stb", '3', _ <= 'B', reqState=AltState1)(ramPrinter),
    new WordOp("stop", "00".b)(wordPrinter),
    new HalfOp("stw", '3', _ <= 'B')(ramPrinter),
    new HalfOp("sub", '6')(regPrinter),
    new HalfOp("sub", '6', reqState=AltState2)(immPrinter),
    new WordOp("swap", "4D".b)(wordPrinter),
    new HalfOp("to", '1')(regPrinter),
    new HalfOp("umult", '8', reqState=AltState1)(regPrinter),

    new HalfOp("with", '2')(regPrinter)


  ).sortWith { case (o1, o2) =>
     o1.reqState.value > o2.reqState.value
  }

  def parse(hex:Vector[Byte]):Either[String, Vector[ParsedOp]] = {
    var state:AltState = AltNone
    var rest = hex
    val result = ArrayBuffer[ParsedOp]()
    while (rest.length > 0) {
      import scala.util.control.Breaks._
      breakable {
        val prevLength = rest.length
        for (op <- Parsing.All) {
          op.process(state, rest) match {
            case Some((parsed, newState, newRest)) =>
              result += parsed
              state = newState
              rest = newRest
              break()
            case _ =>
              ()
          }
        }
        if (rest.length == prevLength) {
          return Left("Parse error: no instruction matching at " + rest.mkString(" "))
        }
      }
    }
    Right(result.toVector)
  }
}
