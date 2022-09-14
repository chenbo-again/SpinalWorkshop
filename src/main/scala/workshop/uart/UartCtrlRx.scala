package workshop.uart

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// 滑动窗口采样异步信号，采样频率必须是信号频率的整数倍

case class UartRxGenerics( preSamplingSize: Int = 1,
                           samplingSize: Int = 5,
                           postSamplingSize: Int = 2){

  val rxdSamplePerBit = preSamplingSize + samplingSize + postSamplingSize
  require(isPow2(rxdSamplePerBit))

  if ((samplingSize % 2) == 0)
    SpinalWarning(s"It's not nice to have a even samplingSize value at ${ScalaLocated.short} (because of the majority vote)")
}

// 系统是工作在 sample 的频率下
case class UartCtrlRx(generics : UartRxGenerics) extends Component{
  import generics._  //Allow to directly use generics attribute without generics. prefix
  val io = new Bundle{
    val rxd  = in Bool()
    val samplingTick = in Bool()
    val read = master Flow(Bits(8 bits))
  }

  // Implement the rxd sampling with a majority vote over samplingSize bits
  // Provide a new sampler.value each time sampler.tick is high
  val sampler = new Area {
    val samples = History(
      that  = io.rxd,
      range = 2 until 2+samplingSize,
      when  = io.samplingTick,
      init  = True
    )
    val value       = RegNext(MajorityVote(samples)) init(True)
    val tick        = RegNext(io.samplingTick) init(False)
  }

  // Provide a bitTimer.tick each rxSamplePerBit
  // reset() can be called to recenter the counter over a start bit.
  val bitTimer = new Area {
    val counter  = Reg(UInt(log2Up(rxdSamplePerBit) bit))
    val recenter = False
    val tick = False
    when(sampler.tick) {
      counter := counter - 1
      when(counter === 0) {
        tick := True
      }
    }
    when(recenter){
      counter := preSamplingSize + (samplingSize - 1) / 2 - 1
    }
  }

  // Provide bitCounter.value that count up each bitTimer.tick, Used by the state machine to count data bits and stop bits
  // reset() can be called to reset it to zero
  val bitCounter = new Area {
    val value = Reg(UInt(3 bits))
    val clear = False

    when(bitTimer.tick) {
      value := value + 1
    }
    when(clear){
      value := 0
    }
  }

  // Statemachine that use all precedent area
  val stateMachine = new StateMachine {
    val IDLE  = new State with EntryPoint
    val START = new State
    val DATA  = new State
    val STOP  = new State

    val buffer = Reg(Bits(8 bits)) init (0)

    io.read.payload := buffer
    io.read.valid := False

    IDLE.whenIsActive{
      when(bitTimer.tick && !sampler.value) {
        bitCounter.clear := True
        goto(START)
      }
    }

    START.whenIsActive{
      //? buffer 赋值条件和 bitCounter 赋值条件一致
      when(bitTimer.tick) {
        buffer(bitCounter.value) := sampler.value

        when(bitCounter.value === 7) {
          goto(DATA)
        }
      }
    }

    DATA.whenIsActive {
      //! 只能有效一周期
      io.read.valid := True
      
      goto(STOP)
    }

    STOP.whenIsActive {
      when(bitTimer.tick && !sampler.value) {
          bitCounter.clear := True
          goto(START)
      }
    }

  }
}
