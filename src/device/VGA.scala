package ysyx

import chisel3._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

class VGAIO extends Bundle {
  val vgalcd_r_o = Output(UInt(5.W))
  val vgalcd_g_o = Output(UInt(6.W))
  val vgalcd_b_o = Output(UInt(5.W))
  val vgalcd_hsync_o = Output(Bool())
  val vgalcd_vsync_o = Output(Bool())
  val vgalcd_pclk_o = Output(Bool())
  val vgalcd_de_o = Output(Bool())
  val irq_o = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val apb4_pclk = Input(Clock())
  val apb4_presetn = Input(Bool())
  val axi4_aclk = Input(Clock())
  val axi4_aresetn = Input(Bool())
  val apb4 = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val axi4 = new AXI4Bundle(CPUAXI4BundleParameters())
  val vgalcd = new VGAIO
}

class axi4_vgalcd extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class VGACtrlDevIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlDevIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters)
  extends APB4DevTemplate(address, new VGAIO)((in: APBBundle, outer: LazyModuleImp, irq_o: Bool, extra, _, _) => {
  val mvga = Module(new vga_top_apb)
  mvga.io.clock := outer.clock
  mvga.io.reset := outer.reset
  mvga.io.in <> in
  extra <> mvga.io.vga
})

class VGAWrapper(address: Seq[AddressSet], idBits: Int)(implicit p: Parameters) extends LazyModule {
  val apbSlaveNode = APBSlaveNodeGenerator(address)
  val axiMasterNode = AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "vga",
        id   = IdRange(0, 1 << idBits - 1), // Use one bit to distinguish Device IDs
        maxFlight = Some(1)
      )))))
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = apbSlaveNode.in(0)
    val (out, _) = axiMasterNode.out(0)
    val vgaIO = IO(new VGAIO)
    val mvga = Module(new axi4_vgalcd)
    in <> mvga.io.apb4
    mvga.io.apb4_pclk := clock
    mvga.io.apb4_presetn := !reset.asBool
    mvga.io.axi4_aclk := clock
    mvga.io.axi4_aresetn := !reset.asBool
    mvga.io.axi4 <> out
    mvga.io.vgalcd <> vgaIO
  }
}
