package com.deweyvm.gsudis

object Test {
  var tests = 0
  def test(input:String, expected:String) {
    import Parsing._
    val hex = input.split(" ").map{_.b}.toVector
    val parsed: Either[String, Vector[ParseResult]] = parse(hex)
    tests += 1
    parsed match {
      case Right(rest :+ x) =>
        assert(x.toString.trim() == expected, "got (%s) expected (%s)" format (x, expected))
        println("[PASS] \"%s\" => \"%s\"" format (input, expected))
      case Left(a) =>
        System.err.println(a)
        throw new Exception("test failure")
    }
  }

  def testReg(range:Range)(pre:String, exp:String, parens:Boolean=false) = {
    range foreach { i:Int =>
      val hex = "%X" format i
      val expected = if(parens) {
        exp + " (r" + i + ")"
      } else {
        exp + " r" + i
      }

      test(pre + hex, expected)
    }
  }

  def testImm(range:Range)(pre:String, exp:String) = {
    range foreach { i:Int =>
      val hex = "%X" format i
      test(pre + hex, exp + " #$" + OpPrinter.padLeft(2, hex))
    }
  }

  def testRegImm(regRange:Range)(immRange:Range)(pre:String, exp:String) = {
    regRange foreach { i:Int =>
      immRange foreach { k:Int =>
        val regHex = "%X" format i
        val imm = "%02X" format k

        test(pre + regHex + " " + imm, "%s r%d,#$%s" format (exp, i,OpPrinter.padLeft(2, imm)) )
      }
    }
  }

  def testLms(pre:String, exp:String) = {
    (0 until 16) foreach { i:Int =>
      (0 until 256) foreach { k:Int =>
        val regHex = "%X" format i
        val imm = "%02X" format k

        test(pre + regHex + " " + imm, "%s r%d,(%s)" format (exp, i,imm) )
      }
    }
  }

  def testRegAdr(regRange:Range)(immRange:Range)(pre:String, exp:String) = {
    regRange foreach { i:Int =>
      immRange foreach { k:Int =>
        val regHex = "%X" format i
        val imm = "%02X" format k

        test(pre + regHex + " " + imm, "%s r%d,$%s" format (exp, i,OpPrinter.padLeft(2, imm)) )
      }
    }
  }

  def testSms(pre:String, exp:String) = {
    (0 until 16) foreach { i:Int =>
      (0 until 256) foreach { k:Int =>
        val regHex = "%X" format i
        val imm = "%02X" format k

        test(pre + regHex + " " + imm, "%s (%s),r%d" format (exp, imm, i) )
      }
    }
  }

  def testRegImm2(regRange:Range)(immRange:Range)(pre:String, exp:String) = {
    regRange foreach { i:Int =>
      immRange foreach { k:Int =>
        immRange foreach { p:Int =>
          val regHex = "%X" format i
          val imm = "%02X" format k
          val imm2 = "%02X" format p
          test(pre + regHex + " " + imm + " " + imm2, "%s r%d,#$%s%s" format (exp, i,imm2, imm) )
        }

      }
    }
  }

  def testRegAdr2(regRange:Range)(immRange:Range)(pre:String, exp:String, before:Boolean) = {
    regRange foreach { i:Int =>
      immRange foreach { k:Int =>
        immRange foreach { p:Int =>
          val regHex = "%X" format i
          val imm = "%02X" format k
          val imm2 = "%02X" format p
          val expected = if (before) {
            "%s (%s%s),r%d" format (exp, imm2, imm, i)
          } else {
            "%s r%d,(%s%s)" format (exp, i, imm2, imm)
          }
          test(pre + regHex + " " + imm + " " + imm2, expected)
        }

      }
    }
  }

  def testMove(name:String, first:String, second:String) {
    (0 until 16) foreach { i =>
      (0 until 16) foreach { k =>
        val fb = first + "%X" format i
        val sb = second + "%X" format k
        test(fb + " " + sb, name + " r" + i + ",r" + k)
      }
    }

  }

  def testReg16Imm256 = testRegImm(0 until 16)(0 until 256) _
  def testReg16Adr256 = testRegAdr(0 until 16)(0 until 256) _
  def testReg16(pre:String, exp:String) = testReg(0 until 16)(pre, exp)
  def testImm16 = testImm(0 until 16) _

  def testBranch(op:String, name:String) {
    (0 until 255) foreach { k =>
      val hex = "%02X" format k
      val output = Parsing.to2comp(k)
      test(op + " " + hex, name + " " +output)
    }
  }

  def runAll() {
    val start = System.nanoTime
    testReg16("3D 5", "adc")
    testImm16("3F 5", "adc")
    testReg16("5", "add")
    testImm16("3E 5", "add")
    test("3D", "alt1")
    test("3E", "alt2")
    test("3F", "alt3")
    testReg(1 until 16)("7", "and")
    testImm(1 until 16)("3E 7", "and")
    test("96", "asr")
    testBranch("0C", "bcc")
    testBranch("0D", "bcs")
    testBranch("09", "beq")
    testBranch("07", "bge")
    testReg(1 until 16)("3D 7", "bic")
    testImm(1 until 16)("3F 7", "bic")
    testBranch("06", "blt")
    testBranch("0B", "bmi")
    testBranch("08", "bne")
    testBranch("0A", "bpl")
    testBranch("05", "bra")
    testBranch("0E", "bvc")
    testBranch("0F", "bvs")
    test("02", "cache")
    test("3D 4E", "cmode")
    testReg16("3F 6", "cmp")
    test("4E", "color")
    testReg(0 until 15)("E", "dec")
    test("3D 96", "div2")
    testReg16("B", "from")
    test("EF", "getb")
    test("3D EF", "getbh")
    test("3E EF", "getbl")
    test("3F EF", "getbs")
    test("DF", "getc")
    test("C0", "hib")
    testReg16Imm256("A", "ibt")
    testReg(0 until 15)("D", "inc")
    testRegImm2(0 until 16)(0 until 256)("F", "iwt")
    testReg(8 to 13)("9", "jmp")
    testReg(0 to 11)("3D 4", "ldb", true)
    testReg(0 to 11)("4", "ldw", true)
    testImm(1 to 4)("9", "link")
    testReg(8 to 13)("3D 9", "ljmp")
    testRegAdr2(0 until 16)(0 until 256)("3D F", "lm", false)
    testLms("3D A", "lms")
    test("3D 9F", "lmult")
    test("9E", "lob")
    test("3C", "loop")
    test("03", "lsr")
    test("70", "merge")
    testMove("move", "2", "1")
    testMove("moves", "2", "B")
    testReg16("8", "mult")
    testImm16("3E 8", "mult")
    test("01", "nop")
    test("4F", "not")
    testReg(1 until 16)("C", "or")
    testImm(1 until 16)("3E C", "or")
    test("4C", "plot")
    test("3E DF", "ramb")
    test("04", "rol")
    test("3F DF", "romb")
    test("97", "ror")
    test("3D 4C", "rpix")
    testReg16("3D 6", "sbc")
    test("90", "sbk")
    test("95", "sex")

    testRegAdr2(0 until 16)(0 until 256)("3E F", "sm", true)
    testSms("3E A", "sms")
    testReg(0 to 11)("3D 3", "stb", true)
    test("00", "stop")
    testReg(0 to 11)("3", "stw", true)
    testReg16("6", "sub")
    testImm16("3E 6", "sub")
    test("4D", "swap")
    testReg16("1", "to")
    testReg16("3D 8", "umult")
    val end = System.nanoTime
    println("Passed %d tests in %.2f seconds." format (tests, (end - start).toDouble/1000000000))
  }
}
