package bridge

import bridge.axi.AXI32To64
import bridge.psram.PSRAMWrapper

object BridgeElaborator extends App {
  val firtoolOptions = Array("--disable-annotation-unknown", "--lowering-options=disallowExpressionInliningInPorts")
  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(new PSRAMWrapper(address = 0xa0000000L), args, firtoolOptions)
  _root_.circt.stage.ChiselStage.emitSystemVerilogFile(new AXI32To64, args, firtoolOptions)
}
