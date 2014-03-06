package com.deweyvm.gsudis

abstract class AltState(val value:Int) {
  def >=(other:AltState):Boolean
}

case object AltNone extends AltState(0) {
  override def >=(other:AltState) = other.value == 0
}
case object AltState1 extends AltState(1) {
  override def >=(other:AltState) = other.value <= 1
}
case object AltState2 extends AltState(2) {
  override def >=(other:AltState) = other.value == 2 || other.value == 0
}
case object AltState3 extends AltState(3) {
  override def >=(other:AltState) = true
}
