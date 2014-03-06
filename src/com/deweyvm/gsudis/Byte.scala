package com.deweyvm.gsudis

case class Byte(upper:Char, lower:Char) {
  override def toString = "%s%s" format (upper, lower)
}
