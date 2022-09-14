package workshop.counter

import spinal.core._

case class Counter(width: Int) extends Component {
  val io = new Bundle {
    val clear    = in  Bool()
    val value    = out UInt(width bits)
    val full     = out Bool()
  }

  val _val = Reg(UInt(width bits)) init 0
  _val := _val + 1
  when(io.clear) {
    _val := 0
  }
  
  io.full := _val === ((1<<width) - 1)
  io.value := _val
  
}
