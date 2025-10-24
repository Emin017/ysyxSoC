package device

import chisel3._
import freechips.rocketchip.amba.axi4.{AXI4AdapterNode, AXI4Bundle, AXI4BundleParameters, AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters}
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, InModuleBody, LazyModule, LazyModuleImp, MemoryDevice, RegionType, TransferSizes}
import freechips.rocketchip.subsystem.{ExtIn, ExtMem, MemoryPortParams}
import freechips.rocketchip.util.StringToAugmentedString
import org.chipsalliance.cde.config.Parameters
import ysyx.ChipLinkParam.idBits
import ysyx.{CPUAXI4BundleParameters, ChipLinkParam}

class FpgaDataLane(dataBits: Int) extends Bundle {
  val clk  = Output(Clock())
  val rst  = Output(Bool())
  val send = Output(Bool())
  val data = Output(UInt(dataBits.W))
}
class FpgaIO extends Bundle {
  val c2b = new FpgaDataLane(8)
  val b2c = Flipped(new FpgaDataLane(8))
}

class ChipLinkIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val slave_axi4_mem_0 = Flipped(new AXI4Bundle(CPUAXI4BundleParameters()))
  val mem_axi4_0 = new AXI4Bundle(CPUAXI4BundleParameters())
  val fpga_io = new FpgaIO
}

class ChiplinkBridge extends BlackBox {
  val io = IO(new ChipLinkIO())
}

class ChipLinkWrapper(implicit p: Parameters) extends LazyModule {
  private object toSlave {
    val slavePortParamsOpt = p(ExtIn)
    val portName = "slave_port_axi4_mem"
    val fifoBits = 1
    val idBits = ChipLinkParam.idBits
  }
  val axi4SlaveNode = AXI4MasterNode(
    toSlave.slavePortParamsOpt.map(params =>
      AXI4MasterPortParameters(
        masters = Seq(AXI4MasterParameters(
          name = toSlave.portName.kebab,
          id   = IdRange(0, 1 << idBits))))).toSeq)

  private object masterMem {
    val portName = "axi4"
    val device = new MemoryDevice
    val cacheBlockBytes = 32
    val idBits = ChipLinkParam.idBits
  }

  val axi4MasterMemNode = AXI4SlaveNode(p(ExtMem).map { case MemoryPortParams(memPortParams, nMemoryChannels, _) =>
    Seq.tabulate(nMemoryChannels) { channel =>
      val base = ChipLinkParam.mem
      val filter = AddressSet(channel * masterMem.cacheBlockBytes, ~((nMemoryChannels - 1) * masterMem.cacheBlockBytes))

      AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address = base.intersect(filter).toList,
          resources = masterMem.device.reg,
          regionType = RegionType.UNCACHED, // cacheable
          executable = true,
          supportsWrite = TransferSizes(1, masterMem.cacheBlockBytes),
          supportsRead = TransferSizes(1, masterMem.cacheBlockBytes),
          interleavedId = Some(0))), // slave does not interleave read responses
        beatBytes = memPortParams.beatBytes)
    }
  }.toList.flatten)

  val slave = InModuleBody { axi4SlaveNode.makeIOs() }
  val master_mem = InModuleBody { axi4MasterMemNode.makeIOs() }

  val node = AXI4AdapterNode()
  axi4MasterMemNode := node := axi4SlaveNode

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val fpga_io = IO(new FpgaIO)

    (node.in zip node.out) foreach {
      case ((in, edgeIn), (out, edgeOut)) => {

        val mchiplink = Module(new ChiplinkBridge)

        mchiplink.io.clock := clock
        mchiplink.io.reset := reset

        mchiplink.io.slave_axi4_mem_0.exclude(
          _.ar.bits.addr, _.aw.bits.addr
        ) :<>= in.exclude(
          _.ar.bits.addr, _.aw.bits.addr
        )
        mchiplink.io.slave_axi4_mem_0.ar.bits.addr := in.ar.bits.addr - "h60000000".U(32.W)
        mchiplink.io.slave_axi4_mem_0.aw.bits.addr := in.aw.bits.addr - "h60000000".U(32.W)

        mchiplink.io.mem_axi4_0 <> out
        mchiplink.io.fpga_io <> fpga_io
      }
    }
  }
}
