package com.deweyvm.gsudis

import java.util.Scanner
import java.io.{File, FileOutputStream, OutputStream}
import scala.util.control.Breaks._

object Main {
  val fileSymbol = ">"
  def streamWrite(stream:OutputStream, s:String) {
    stream.write(s.getBytes("UTF-8"))
  }

  def getStream(raw:String):(String, OutputStream) = {
    if (raw.contains(fileSymbol)) {
      try {
        val s = raw.split(fileSymbol)
        val input = s(0)
        val filename = s(1)
        val file = new File(filename)
        if (file.exists() && !file.delete()) {
          throw new RuntimeException("File %s exists and could not be deleted." format filename)
        }
        (input, new FileOutputStream(file))
      } catch {
        case index:IndexOutOfBoundsException =>
          throw new RuntimeException("Must specify output file after >")
      }
    } else {
      (raw, System.out)
    }
  }

  def writeErr(s:Any) {
    System.err.println(s)
    System.err.flush()
  }

  def run(interactive:Boolean) {
    import Parsing._
    val in = new Scanner(System.in)

    breakable { do {
      if (interactive){
        print("gsudis> ")
      }
      val next = in.nextLine()
      if (next.replace(" ", "").length == 0) {
        //no input, do nothing
      } else {
        try {
          val (input, stream) = getStream(next)
          val hex = input.toUpperCase.split(" ").map{_.b}.toVector

          parse(hex) match {
            case Left(err) =>
              writeErr(err)
            case Right(res) =>
              val labeled = Labeler(res).process
              labeled foreach { r =>
                val s = r.makeString + System.lineSeparator()
                streamWrite(stream, s)
              }
              stream.flush()
          }
        } catch {
          case nse:NoSuchElementException =>
            writeErr("End of input.")
            writeErr(nse)
            break()
          case s:Exception =>
            writeErr(s)
        }

      }

    } while (interactive) }
  }


  def main(args:Array[String]) {
    if (args.contains("-t")) {
      Test.runAll()
      System.exit(0)
    }

    run(!args.contains("-stdin"))
  }
}
