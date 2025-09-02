package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.system.SimAXIMem

object AXI4SlaveNodeGenerator {
  def apply(params: Option[MasterPortParams], address: Seq[AddressSet])(implicit valName: ValName) =
    AXI4SlaveNode(params.map(p => AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = address,
          executable    = p.executable,
          supportsWrite = TransferSizes(1, p.maxXferBytes),
          supportsRead  = TransferSizes(1, p.maxXferBytes))),
        beatBytes = p.beatBytes
      )).toSeq)
}

class ysyxSoCASIC(implicit p: Parameters) extends LazyModule {
  val xbar = AXI4Xbar()
  val apbxbar = LazyModule(new APBFanout).node
  val cpu = LazyModule(new CPU(idBits = ChipLinkParam.idBits))
  val chipMaster = if (Config.hasChipLink) Some(LazyModule(new ChipLinkMaster)) else None
  val chiplinkNode = if (Config.hasChipLink) Some(AXI4SlaveNodeGenerator(p(ExtBus), ChipLinkParam.allSpace)) else None

  def AddrSpace(base: BigInt, len: BigInt = 0x1000) = AddressSet.misaligned(base, len)

  // RISC-V system
  val lclint    = LazyModule(new APB4CLINT   (AddrSpace(0x02010000, 0x10000)))

  // generic system
  val luart0    = LazyModule(new APBUart16550(AddrSpace(0x10000000, 0x8)))
  val lspi      = LazyModule(new APBSPI      (AddrSpace(0x10001000, 0x20)   ++     // SPI controller
                                              AddrSpace(0x30000000, 0x10000000)))  // XIP flash
  val larchinfo = LazyModule(new APB4ArchInfo(AddrSpace(0x10006000, 0x10)))

  // interface
  val lgpio0    = LazyModule(new APB4GPIO    (AddrSpace(0x10100000, 0x40)))
  val lgpio1    = LazyModule(new APB4GPIO    (AddrSpace(0x10101000, 0x40)))
  val lgpio2    = LazyModule(new APB4GPIO    (AddrSpace(0x10102000, 0x40)))
  val li2c      = LazyModule(new APB4I2C     (AddrSpace(0x10104000, 0x20)))
  val ltim0     = LazyModule(new APB4Timer   (AddrSpace(0x10108000, 0x20)))
  val ltim1     = LazyModule(new APB4Timer   (AddrSpace(0x10109000, 0x20)))
  val ltim2     = LazyModule(new APB4Timer   (AddrSpace(0x1010a000, 0x20)))
  val ltim3     = LazyModule(new APB4Timer   (AddrSpace(0x1010b000, 0x20)))

  // application
  val lcrc      = LazyModule(new APB4CRC     (AddrSpace(0x10301000, 0x20)))

  // memory
  val sdramAddressSet = AddrSpace(0x80000000L, 0x2000000)
  val lsdram_apb = if (!Config.sdramUseAXI) Some(LazyModule(new APBSDRAM (sdramAddressSet))) else None
  val lsdram_axi = if ( Config.sdramUseAXI) Some(LazyModule(new AXI4SDRAM(sdramAddressSet))) else None

  // homework
  val lgpio     = if (Config.hasHomeWork) Some(LazyModule(new APBGPIO    (AddrSpace(0x10002000, 0x10)))) else None
  val lkeyboard = if (Config.hasHomeWork) Some(LazyModule(new APBKeyboard(AddrSpace(0x10011000, 0x8)))) else None
  val lvga      = if (Config.hasHomeWork) Some(LazyModule(new APBVGA     (AddrSpace(0x21000000, 0x200000)))) else None
  val lpsram    = if (Config.hasHomeWork) Some(LazyModule(new APBPSRAM   (AddrSpace(0xa0000000L, 0x400000)))) else None

  List(lclint,
       lspi, luart0, larchinfo,
       lgpio0, lgpio1, lgpio2, li2c, ltim0, ltim1, ltim2, ltim3,
       lcrc
  ).map(_.node := apbxbar)

  if (Config.isDstage) {
    apbxbar := APBDelayer() := AXI4ToAPB() := AXI4Buffer() := xbar
  } else if (Config.hasHomeWork) {
    val xbar2 = AXI4Xbar()
    List(lpsram, lgpio, lkeyboard, lvga).map(_.get.node := apbxbar)
    apbxbar := APBDelayer() := AXI4ToAPB() := AXI4Buffer() := xbar2
    val lmrom = LazyModule(new AXI4MROM(AddrSpace(0x20000000, 0x1000)))
    val sramNode = AXI4RAM(AddrSpace(0x02020000, 0x2000).head, false, true, 4, None, Nil, false)
    List(lmrom.node, sramNode).map(_ := xbar2)
    xbar2 := AXI4UserYanker(Some(1)) := AXI4Fragmenter() := xbar
  } else {
    apbxbar := APBDelayer() := AXI4ToAPB() := AXI4UserYanker(Some(1)) := AXI4Fragmenter() := xbar
  }

  if (Config.sdramUseAXI && !Config.isDstage) lsdram_axi.get.node := ysyx.AXI4Delayer() := xbar
  else                                        lsdram_apb.get.node := apbxbar

  if (Config.hasChipLink) chiplinkNode.get := xbar
  xbar := cpu.masterNode

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with DontTouch {
    // generate delayed reset for cpu, since chiplink should finish reset
    // to initialize some async modules before accept any requests from cpu
    cpu.module.reset := SynchronizerShiftReg(reset.asBool, 10) || reset.asBool

    val fpga_io = if (Config.hasChipLink) Some(IO(chiselTypeOf(chipMaster.get.module.fpga_io))) else None
    if (Config.hasChipLink) {
      // connect chiplink slave interface to crossbar
      (chipMaster.get.slave zip chiplinkNode.get.in) foreach { case (io, (bundle, _)) => io <> bundle }

      // connect chiplink dma interface to cpu
      cpu.module.io_slave <> chipMaster.get.master_mem(0)

      // expose chiplink fpga I/O interface as ports
      fpga_io.get <> chipMaster.get.module.fpga_io
    } else {
      cpu.module.io_slave := DontCare
    }

    // external slower clock
    val clock_half = IO(Input(Bool()))

    List(ltim0, ltim1, ltim2, ltim3).map { t =>
      t.module.extra.capch_i := false.B
      t.module.extra.exclk_i := clock_half
    }
    List(lgpio0, lgpio1, lgpio2).map { t =>
      t.module.extra.gpio_in_i := 0.U
      t.module.extra.gpio_alt_0_out_i := 0.U
      t.module.extra.gpio_alt_0_dir_i := 0.U
      t.module.extra.gpio_alt_1_out_i := 0.U
      t.module.extra.gpio_alt_1_dir_i := 0.U
    }

    val i2c_io = li2c.module.extra
    val i2c_scl = IO(Analog(1.W))
    val i2c_sda = IO(Analog(1.W))
    i2c_io.scl_i := TriStateInBuf(i2c_scl, i2c_io.scl_o, i2c_io.scl_dir_o)
    i2c_io.sda_i := TriStateInBuf(i2c_sda, i2c_io.sda_o, i2c_io.sda_dir_o)

    // connect interrupt signal to cpu
    val intr_from_chipSlave = IO(Input(Bool()))
    cpu.module.io_interrupt := intr_from_chipSlave

    val sdramBundle = if (Config.sdramUseAXI) lsdram_axi.get.module.sdram_bundle
                      else                    lsdram_apb.get.module.extra

    // expose slave I/O interface as ports
    def genIO[T <: Data](name: String, inner: T) = {
      val outer = IO(chiselTypeOf(inner))
      outer.suggestName(name)
      outer <> inner
      outer
    }
    def genAPB4DevIO[T <: Data](name: String, lmodule: APB4DevTemplate[T]) = genIO(name, lmodule.module.extra)
    def genSomeAPB4DevIO[T <: Data](name: String, lmodule: Option[APB4DevTemplate[T]]) = {
      if (Config.hasHomeWork) Some(genAPB4DevIO(name, lmodule.get)) else None
    }

    val uart  = genAPB4DevIO("uart", luart0)
    val spi   = genAPB4DevIO("spi", lspi)
    val sdram = genIO("sdram", sdramBundle)
    val psram = genSomeAPB4DevIO("psram", lpsram)
    //val gpio  = genSomeAPB4DevIO("gpio", lgpio)
    val ps2   = genSomeAPB4DevIO("ps2", lkeyboard)
    val vga   = genSomeAPB4DevIO("vga", lvga)
    val gpio  = genIO("gpio", lgpio0.module.extra.gpio_out_o)
  }
}

class ysyxSoCFPGA(implicit p: Parameters) extends ChipLinkSlave


class ysyxSoCFull(implicit p: Parameters) extends LazyModule {
  val asic = LazyModule(new ysyxSoCASIC)
  ElaborationArtefacts.add("graphml", graphML)

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with DontTouch {
    val masic = asic.module

    if (Config.hasChipLink) {
      val fpga = LazyModule(new ysyxSoCFPGA)
      val mfpga = Module(fpga.module)
      masic.dontTouchPorts()

      masic.fpga_io.get.b2c <> mfpga.fpga_io.c2b
      mfpga.fpga_io.b2c <> masic.fpga_io.get.c2b

      (fpga.master_mem zip fpga.axi4MasterMemNode.in).map { case (io, (_, edge)) =>
        val mem = LazyModule(new SimAXIMem(edge,
          base = ChipLinkParam.mem.base, size = ChipLinkParam.mem.mask + 1))
        Module(mem.module)
        mem.io_axi4.head <> io
      }

      fpga.master_mmio.map(_ := DontCare)
      fpga.slave.map(_ := DontCare)
    }

    // slower clock
    val divReg = RegInit(false.B)
    divReg := !divReg
    masic.clock_half := divReg

    masic.intr_from_chipSlave := false.B

    val gpio_led = Module(new gpio_led_model)
    gpio_led.io.led_i := masic.gpio

    val flash = Module(new flash)
    flash.io <> masic.spi
    flash.io.ss := masic.spi.ss(0)
    val bitrev = Module(new bitrev)
    bitrev.io <> masic.spi
    bitrev.io.ss := masic.spi.ss(7)
    masic.spi.miso := List(bitrev.io, flash.io).map(_.miso).reduce(_&&_)

    val sdram = Module(new sdramChisel)
    sdram.io <> masic.sdram

    val externalPins = IO(new Bundle{
      val uart = chiselTypeOf(masic.uart)
      //val gpio = if (Config.hasHomeWork) Some(chiselTypeOf(masic.gpio.get)) else None
      val ps2  = if (Config.hasHomeWork) Some(chiselTypeOf(masic.ps2.get))  else None
      val vga  = if (Config.hasHomeWork) Some(chiselTypeOf(masic.vga.get))  else None
    })
    externalPins.uart <> masic.uart

    if (Config.hasHomeWork) {
      val psram = Module(new psramChisel)
      psram.io <> masic.psram.get

      //externalPins.gpio.get <> masic.gpio.get
      externalPins.ps2.get <> masic.ps2.get
      externalPins.vga.get <> masic.vga.get
    }
  }
}
