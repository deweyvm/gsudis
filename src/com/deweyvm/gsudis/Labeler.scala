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

case class InvalidBranch(op:ParsedOp) extends Code {
  val bytes = op.numBytes
  def makeString = op.toString
}

case class Label(name:String) extends Code {
  val bytes = 0
  def makeString = "%s" format name
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

/**
 * Converts branch instructions with absolute offsets to use labels.
 */
case class Labeler(instrs:Vector[ParsedOp]) {
  import Labeler._

  def process:Vector[Code] = processHelper(opsToCodes(instrs), "label", 0)

  def insert[T](i:Int, e:T, v:Vector[T]):Vector[T] = v.patch(i, Vector(e), 0)

  private def processHelper(instrs:Vector[Code], prefix:String, num:Int):Vector[Code] = {
    val branchIndex = instrs.indexWhere(isRawBranch)
    val labelName = ".%s%02d" format (prefix, num)
    val label = Label(labelName)
    if (branchIndex == -1) {
      instrs
    } else {
      val branch = instrs(branchIndex)
      val op = branch.asInstanceOf[Instruction].op
      val branchAmount = Parsing.to2comp(Integer.parseInt(op.args(0), 16))
      if (branchAmount == 0) {
        val updated = instrs.updated(branchIndex, Branch(op, labelName))
        processHelper(insert(branchIndex + 1, label, updated), prefix, num + 1)
      } else {
        val k = findPos(instrs, branchIndex, branchAmount)
        val replaced = k match {
          case None =>
            instrs.updated(branchIndex, InvalidBranch(op))
          case Some(pos) =>
            val updated = instrs.updated(branchIndex, Branch(op, labelName))
            insert(pos, label, updated)
        }
        processHelper(replaced, prefix, num + 1)
      }

    }
  }

  def findPos(instrs:Vector[Code], branchIndex:Int, branchAmount:Int):Option[Int] = {
    val offset = if (branchAmount > 0) 0 else 1
    val range = if (branchAmount > 0) (branchIndex + 1) until instrs.length else (0 until (branchIndex + 1)).reverse
    var byteCount = 0
    for (k <- range) {
      if (byteCount == math.abs(branchAmount)) {
        return Some(k + offset)
      }
      val i = instrs(k)
      byteCount += i.bytes
    }
    None
  }
}
