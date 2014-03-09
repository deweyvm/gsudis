package com.deweyvm.gsudis

import java.util.Scanner
import java.io.{File, FileOutputStream, OutputStream}


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
  def main(args:Array[String]) {
    import Parsing._
    val in = new Scanner(System.in)

    if (args.contains("-t")) {
      Test.runAll()
      exit(0)
    }
    while (true) {
      try {
        print("gsudis> ")
        val next = in.nextLine()
        val (input, stream) = getStream(next)
        val hex = input.toUpperCase.split(" ").map{_.b}.toVector

        parse(hex) match {
          case Left(err) =>
            System.err.println(err)
            System.err.flush()
          case Right(res) => res foreach { r =>
            val s = r.toString
            streamWrite(stream, s)

          }
          streamWrite(stream, "\n")
          stream.flush()
        }
      } catch {
        case s:Exception =>
          System.err.println(s)
          System.err.flush()
      }
    }
  }
}
