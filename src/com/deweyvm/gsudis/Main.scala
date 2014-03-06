package com.deweyvm.gsudis





case class ParsedOp(op:String, args:Vector[String])

object Op {
  implicit class string2Byte(s:String) {
    def b:Byte = {
      val upper = s(0)
      val lower = s(1)
      Byte(upper, lower)
    }
  }
  val immPrinter = Printer() + OpPrinter.imm
  val regPrinter = Printer() + OpPrinter.reg
  val opPrinter = Printer()
  val adrPrinter = Printer() + OpPrinter.adr
  val regImmPrinter = Printer() + OpPrinter.reg + OpPrinter.imm
  val regImm2Printer = Printer() + OpPrinter.reg + OpPrinter.imm + OpPrinter.imm
  val regAdrPrinter = Printer() + OpPrinter.reg + OpPrinter.adr
  val regAdr2Printer = Printer() + OpPrinter.reg + OpPrinter.adr + OpPrinter.adr
  val movePrinter = Printer() + OpPrinter.reg + OpPrinter.reg
  val All:Vector[OpParser with OpPrinter] = Vector(
    new HalfOp("add",  '5')(regPrinter),
    new HalfOp("addi", '5', reqState=AltState2)(immPrinter),
    new HalfOp("adc",  '5', reqState=AltState1)(regPrinter),
    new HalfOp("adci", '5', reqState=AltState3)(immPrinter),
    new WordOp("alt1", "3D".b, newState=AltState1)(opPrinter),
    new WordOp("alt2", "3E".b, newState=AltState2)(opPrinter),
    new WordOp("alt3", "3F".b, newState=AltState3)(opPrinter),
    new HalfOp("and", '7', _ >= '1')(regPrinter),
    new HalfOp("andi",'7', _ >= '1', reqState=AltState2)(immPrinter),
    new WordOp("asr", "96".b)(opPrinter),
    new ArgOp("bcc", "0C".b)(adrPrinter),
    new ArgOp("bcs", "0D".b)(adrPrinter),
    new ArgOp("beq", "09".b)(adrPrinter),
    new ArgOp("bge", "07".b)(adrPrinter),
    new HalfOp("bic", '7', _ >= '1', reqState=AltState1)(regPrinter),
    new HalfOp("bici",'7', _ >= '1', reqState=AltState3)(immPrinter),
    new ArgOp("blt", "06".b)(adrPrinter),
    new ArgOp("bmi", "0B".b)(adrPrinter),
    new ArgOp("bne", "08".b)(adrPrinter),
    new ArgOp("bra", "05".b)(adrPrinter),
    new ArgOp("bvc", "0E".b)(adrPrinter),
    new ArgOp("bvs", "0E".b)(adrPrinter),
    new WordOp("cache", "02".b)(opPrinter),
    new WordOp("cmode", "4E".b, reqState=AltState1)(opPrinter),
    new HalfOp("cmp", '6', reqState=AltState3)(regPrinter),
    new WordOp("color", "4E".b)(opPrinter),
    new HalfOp("dec", 'E', _ <= 'E')(regPrinter),
    new WordOp("div2", "96".b, reqState=AltState1)(opPrinter),
    new WordOp("fmult","9F".b)(opPrinter),
    new HalfOp("from", 'B')(regPrinter),
    new WordOp("getb", "EF".b)(opPrinter),
    new WordOp("getbh", "EF".b, reqState=AltState1)(opPrinter),
    new WordOp("getbl", "EF".b, reqState=AltState2)(opPrinter),
    new WordOp("getbs", "EF".b, reqState=AltState3)(opPrinter),
    new WordOp("getc", "DF".b)(opPrinter),
    new WordOp("hib", "C0".b)(opPrinter),
    new RegArgOp("itb", 'A')(regImmPrinter),
    new HalfOp("inc", 'D', _ <= 'E')(regPrinter),
    new RegArg2Op("iwt", 'F')(regImm2Printer),
    new HalfOp("jmp", '9', _ >= '8')(regPrinter),
    new HalfOp("ldb", '4', _ <= 'B', reqState=AltState1)(regPrinter),
    new HalfOp("ldw", '4', _ <= 'B')(regPrinter),
    //same as iwt??//new RegArg2Op("lea", 'F')(regImm2Printer),
    new HalfOp("link", '9', x => x >= '1' && x <= '4')(immPrinter),
    new HalfOp("ljmp", '9', x => x >= '8' && x <= 'D')(regPrinter),
    new RegArg2Op("lm", 'F', reqState=AltState1)(regAdr2Printer),
    new RegArgOp("lms", 'A', reqState=AltState1)(regImmPrinter),
    new WordOp("lmult", "9F".b, reqState=AltState1)(opPrinter),
    new WordOp("lob", "9E".b)(opPrinter),
    new WordOp("loop", "3C".b)(opPrinter),
    new WordOp("lsr", "03".b)(opPrinter),
    new WordOp("merge", "70".b)(opPrinter),
    new MoveOp("move", '2', '1')(movePrinter),
    new MoveOp("moves", '2', 'B')(movePrinter),
    new HalfOp("mult", '8')(regPrinter),
    new HalfOp("multi", '8', reqState=AltState2)(immPrinter),
    new WordOp("nop", "01".b)(opPrinter),
    new WordOp("not", "4F".b)(opPrinter),
    new HalfOp("or", 'C', _ >= '1')(regPrinter),
    new HalfOp("ori", 'C',_ >= '1',  reqState=AltState2)(immPrinter),
    new WordOp("plot", "4C".b)(opPrinter),
    new WordOp("ramb", "4C".b, reqState=AltState2)(opPrinter),
    new WordOp("rol", "04".b)(opPrinter),
    new WordOp("romb", "DF".b, reqState=AltState3)(opPrinter),
    new WordOp("ror", "97".b)(opPrinter),
    new WordOp("rpix", "4C".b, reqState=AltState1)(opPrinter),
    new HalfOp("sbc", '6', reqState=AltState1)(regPrinter),
    new WordOp("sbk", "90".b)(opPrinter),
    new WordOp("sex", "95".b)(opPrinter),
    new RegArg2Op("sm", 'F', reqState=AltState2)(regAdr2Printer),
    new RegArgOp("sms", 'A', reqState=AltState2)(regAdrPrinter),
    new HalfOp("stb", '3', _ <= 'B')(regPrinter),
    new WordOp("stop", "00".b)(opPrinter),
    new HalfOp("stw", '3', _ <= 'B')(regPrinter),
    new HalfOp("sub", '6')(regPrinter),
    new HalfOp("subi", '6', reqState=AltState2)(immPrinter),
    new WordOp("swap", "4D".b)(opPrinter),
    new HalfOp("to", '1')(regPrinter),
    new HalfOp("umul", '8', reqState=AltState1)(regPrinter),

    new HalfOp("with", '2')(regPrinter)


  )/*.sortWith { case (o1, o2) =>
     o1.reqState.value > o2.reqState.value
  }*/
}

object Main {
  def main(args:Array[String]) {
    import Op._
    val hex = "3D 61 3D 81 4D 3E 61 61 3B 00 95 3D 4C 90 3F DF 3E 4C 4C 3E C2 C2 4F 3E 81 01 85 03 3C 9E 3D 9F 45 D5 C0 DF EF 3D EF 3F EF 3E EF 3D 51 3D EF 9F EF E1 E2 4E 3F 61 3D 4E 02 02 0E 07 05 06 08 08 0B 23 06 22 3D 71 3F 71 0D FF 09 FF 07 FF 0C FF 71 3E 71 3F 55 3D 51 3E 5F B2 15 51 4E 3D 96 04 97 98 4B 4C 91 92 93 94 98 99 9A 9B 9C 9D 3D 3B A2 12 3D A2 15 3E A5 FF F2 12 34 3D F1 12 34 3E FF AB CD 21 11 21 B1".split(" ")
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
          throw new Exception("Failed to find a parser for bytes starting at " + rest)
        }
      }
    }
  }
}
