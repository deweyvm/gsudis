package com.deweyvm.gsudis

import scala.collection.mutable.ArrayBuffer



object Main {

  def main(args:Array[String]) {
    import Parsing._
    val hex = "3D 61 3D 81 4D 3E 61 61 3B 00 95 3D 4C 90 3F DF 3E 4C 4C 3E C2 C2 4F 3E 81 01 85 03 3C 9E 3D 9F 45 D5 C0 DF EF 3D EF 3F EF 3E EF 3D 51 3D EF 9F EF E1 E2 4E 3F 61 3D 4E 02 02 0E 07 05 06 08 08 0B 23 06 22 3D 71 3F 71 0D FF 09 FF 07 FF 0C FF 71 3E 71 3F 55 3D 51 3E 5F B2 15 51 4E 3D 96 04 97 98 4B 4C 91 92 93 94 98 99 9A 9B 9C 9D 3D 3B A2 12 3D A2 15 3E A5 FF F2 12 34 3D F1 12 34 3E FF AB CD 21 11 21 B1".split(" ")
    val a = hex.map {_.b}.toVector
    Test.runAll()
    /*val parsed = parse(a)
    parsed match {
      case Left(err) => System.err.println(err)
      case Right(res) => res foreach (println(_))
    }*/
  }
}
