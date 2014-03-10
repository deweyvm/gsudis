package com.deweyvm.gsudis


trait Code {
  val bytes:Int
  def makeString:String
}
case class Instruction(op:ParsedOp) extends Code {
  val bytes = op.numBytes
  def makeString = op.toString
}

case class Branch(op:ParsedOp, label:String) extends Code {
  val bytes = op.numBytes
  def makeString = "%s %s ; %s" format (op.op, label, op.args(0))
}

case class Label(name:String) extends Code {
  val bytes = 0
  def makeString = "%s:" format name
}


object Labeler {
  val branches = Vector("bcc", "bcs", "beq", "bge", "blt", "bmi", "bne", "bpl", "bra", "bvc", "bvs")
  def isRawBranch(p:Code) = p match {
    case Instruction(i) if branches.contains(i.op) => true
    case _ => false
  }

  def opsToCodes(ops:Vector[ParsedOp]):Vector[Code] = {
    ops map Instruction
  }
}

case class Labeler(instrs:Vector[ParsedOp]) {
  import Labeler._

  def process:Vector[Code] = processHelper(opsToCodes(instrs), "label", 0)

  def insert[T](i:Int, e:T, v:Vector[T]):Vector[T] = v.patch(i, Vector(e), 0)

  //when not found, keep without a label
  private def processHelper(instrs:Vector[Code], prefix:String, num:Int):Vector[Code] = {
    val branchIndex = instrs.indexWhere(isRawBranch)
    if (branchIndex == -1) {
      instrs
    } else {
      val branch = instrs(branchIndex)
      val labelName = "%s%02d" format (prefix, num)
      val label = Label(labelName)
      val op = branch.asInstanceOf[Instruction].op
      val branchAmount = Integer.parseInt(op.args(0))
      val updated = instrs.updated(branchIndex, Branch(op, labelName))
      if (branchAmount == 0) {
        processHelper(insert(branchIndex + 2, Label(labelName), updated), prefix, num + 1)
      } else if (branchAmount > 0) {
        var byteCount = 0
        for (k <- (branchIndex + 2) until instrs.length) {
          val i = instrs(k)
          byteCount += i.bytes
          if (byteCount == branchAmount) {
            return processHelper(insert(k, label, updated), prefix, num + 1)
          }
        }
        return Vector()
      } else/* if (branchAmount < 0)*/ {
        var byteCount = 0
        for (k <- 0 until (branchIndex + 2)) {
          val i = instrs(k)
          byteCount += i.bytes
          if (byteCount == branchAmount) {
            return processHelper(insert(k, label, updated), prefix, num + 1)
          }
        }
        return Vector()
      }

    }
  }
}
