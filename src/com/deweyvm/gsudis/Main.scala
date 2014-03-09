package com.deweyvm.gsudis

import scala.collection.mutable.ArrayBuffer
import java.util.Scanner
import java.io.{File, FileOutputStream, OutputStream}


object Main {
  def getStream(raw:String):(String, OutputStream) = {
    if (raw.contains(">")) {
      try {
        val s = raw.split(">")
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

    //Test.runAll()
    //exit(0)
    while (true) {
      try {
      print("gsudis> ")
      val next = in.nextLine()
      val (input, stream) = getStream(next)
      val hex = input.toUpperCase.split(" ").map{_.b}.toVector

        parse(hex) match {
          case Left(err) => System.err.println(err)
          case Right(res) => res foreach { r =>
            val s = r.toString
            stream.write(s.getBytes("UTF-8"))

          }
          stream.write("\n".getBytes("UTF-8"))
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
