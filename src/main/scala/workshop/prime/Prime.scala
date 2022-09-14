package workshop.prime

import spinal.core._
import spinal.lib._


object Prime{
  //Pure scala function which return true when the number is prime
  def apply(n : Int) =  ! ((2 until n-1) exists (n % _ == 0))

  //Should return True when the number is prime.
  //? 多次调用会采用一个电路的话可以产生一个查找表
  //* pattern: 先生成电路，然后直接用电路的输出
  def apply(n : UInt) : Bool = {
    val lut = 
      (0 until (1 << n.getWidth))
      .map(x => Bool(Prime(x)))
      .asBits()
    lut(n)
  }
}


