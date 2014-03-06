package com.deweyvm.gsudis

import scala.collection.mutable.ArrayBuffer
import java.util.Scanner


object Main {
  /**
   * TODO:
   *     make sure output matches the manual
   * @param args
   */
  def main(args:Array[String]) {
    import Parsing._
    val in = new Scanner(System.in)

    //Test.runAll()
    //exit(0)
    while (true) {
      print("gsudis> ")
      val hex = in.nextLine().toUpperCase.split(" ").map{_.b}.toVector
      try {
        parse(hex) match {
          case Left(err) => System.err.println(err)
          case Right(res) => res foreach (println(_))
        }
      } catch {
        case s:Exception => System.err.println(s)
      }
      //println(in.nextLine())
    }
    /*val parsed = parse(a)
    parsed match {
      case Left(err) => System.err.println(err)
      case Right(res) => res foreach (println(_))
    }*/
  }
}
