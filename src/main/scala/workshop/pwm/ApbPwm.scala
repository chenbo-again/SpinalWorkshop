package workshop.pwm

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib._
import workshop.counter.Counter

//APB configuration class (generic/parameter)
case class ApbConfig(addressWidth : Int,
                     dataWidth    : Int,
                     selWidth     : Int)

//APB interface definition
case class Apb(config: ApbConfig) extends Bundle with IMasterSlave {
  val PSEL = out Bits(config.selWidth bits)
  val PENABLE = out Bool()
  val PWRITE = out Bool()
  val PADDR = out UInt(config.addressWidth bits)
  val PWDATA = out Bits(config.dataWidth bits)
  val PRDATA = in Bits(config.dataWidth bits)
  val PREADY = in Bool()

  override def asMaster(): Unit = {

  }
}

case class ApbPwm(apbConfig: ApbConfig,timerWidth : Int) extends Component{
  require(apbConfig.dataWidth == 32)
  require(apbConfig.selWidth == 1)

  val io = new Bundle{
    val apb = slave (Apb(apbConfig))
    val pwm = out Bool()
  }

  val _reg = new Bundle {
    val enable = Reg(Bool())
    val dutyCycle = Reg(UInt(timerWidth bits))
  }

  val logic = new Area {
    val hCycle = Counter(timerWidth)
    hCycle.io.clear := !_reg.enable
    io.pwm := hCycle.io.value < _reg.dutyCycle
  }
  
  val control = new Area{
    // 更好的方式是总是根据 paddr 读出寄存器的值，根据写入使能来写入

    io.apb.PREADY := True
    io.apb.PRDATA := 0

    when(io.apb.PSEL(0) && io.apb.PENABLE) {
      switch(io.apb.PADDR) {
        is(0) {
          io.apb.PRDATA := _reg.enable.asBits.resized
          when(io.apb.PWRITE) {
            _reg.enable := io.apb.PWDATA(0)
          } 
        }
        is(4) {
          io.apb.PRDATA := _reg.dutyCycle.asBits.resized
          when(io.apb.PWRITE) {
            _reg.dutyCycle := io.apb.PWDATA.asUInt.resized
          } 
        }
      }
    }

  }
}