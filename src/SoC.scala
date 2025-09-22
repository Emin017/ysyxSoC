package ysyx

import chisel3._
import chisel3.util._

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

  val luart = LazyModule(new APBUart16550(AddressSet.misaligned(0x10000000, 0x1000)))
  val lspi  = LazyModule(new APBSPI(
    AddressSet.misaligned(0x10001000, 0x1000) ++    // SPI controller
    AddressSet.misaligned(0x30000000, 0x10000000)   // XIP flash
  ))
  val lgpio     = if (Config.hasHomeWork) Some(LazyModule(new APBGPIO(AddressSet.misaligned(0x10002000, 0x10)))) else None
  val lkeyboard = if (Config.hasHomeWork) Some(LazyModule(new APBKeyboard(AddressSet.misaligned(0x10011000, 0x8)))) else None
  val lvga      = if (Config.hasHomeWork) Some(LazyModule(new APBVGA(AddressSet.misaligned(0x21000000, 0x200000)))) else None
  val lpsram    = if (Config.hasHomeWork) Some(LazyModule(new APBPSRAM(AddressSet.misaligned(0xa0000000L, 0x400000)))) else None

  val sdramAddressSet = AddressSet.misaligned(0x80000000L, 0x2000000)
  val lsdram_apb = if (!Config.sdramUseAXI) Some(LazyModule(new APBSDRAM (sdramAddressSet))) else None
  val lsdram_axi = if ( Config.sdramUseAXI) Some(LazyModule(new AXI4SDRAM(sdramAddressSet))) else None

  List(lspi.node, luart.node).map(_ := apbxbar)
  if (Config.isDstage) {
    apbxbar := APBDelayer() := AXI4ToAPB() := AXI4Buffer() := xbar
  } else if (Config.hasHomeWork) {
    val xbar2 = AXI4Xbar()
    List(lpsram.get.node, lgpio.get.node, lkeyboard.get.node, lvga.get.node).map(_ := apbxbar)
    apbxbar := APBDelayer() := AXI4ToAPB() := AXI4Buffer() := xbar2
    val lmrom = LazyModule(new AXI4MROM(AddressSet.misaligned(0x20000000, 0x1000)))
    val sramNode = AXI4RAM(AddressSet.misaligned(0x02020000, 0x2000).head, false, true, 4, None, Nil, false)
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

    // connect interrupt signal to cpu
    val intr_from_chipSlave = IO(Input(Bool()))
    cpu.module.io_interrupt := intr_from_chipSlave

    val sdramBundle = if (Config.sdramUseAXI) lsdram_axi.get.module.sdram_bundle
                      else                    lsdram_apb.get.module.sdram_bundle

    // expose slave I/O interface as ports
    val spi = IO(chiselTypeOf(lspi.module.spi_bundle))
    val uart = IO(chiselTypeOf(luart.module.uart))
    val sdram = IO(chiselTypeOf(sdramBundle))
    uart <> luart.module.uart
    spi <> lspi.module.spi_bundle
    sdram <> sdramBundle

    val psram = if (Config.hasHomeWork) Some(IO(chiselTypeOf(lpsram.get.module.qspi_bundle)))   else None
    val gpio  = if (Config.hasHomeWork) Some(IO(chiselTypeOf(lgpio.get.module.gpio_bundle)))    else None
    val ps2   = if (Config.hasHomeWork) Some(IO(chiselTypeOf(lkeyboard.get.module.ps2_bundle))) else None
    val vga   = if (Config.hasHomeWork) Some(IO(chiselTypeOf(lvga.get.module.vga_bundle)))      else None
    if (Config.hasHomeWork) {
      psram.get <> lpsram.get.module.qspi_bundle
      gpio.get <> lgpio.get.module.gpio_bundle
      ps2.get <> lkeyboard.get.module.ps2_bundle
      vga.get <> lvga.get.module.vga_bundle
    }
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

    masic.intr_from_chipSlave := false.B

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
      val gpio = if (Config.hasHomeWork) Some(chiselTypeOf(masic.gpio.get)) else None
      val ps2  = if (Config.hasHomeWork) Some(chiselTypeOf(masic.ps2.get))  else None
      val vga  = if (Config.hasHomeWork) Some(chiselTypeOf(masic.vga.get))  else None
    })
    externalPins.uart <> masic.uart

    if (Config.hasHomeWork) {
      val psram = Module(new psramChisel)
      psram.io <> masic.psram.get

      externalPins.gpio.get <> masic.gpio.get
      externalPins.ps2.get <> masic.ps2.get
      externalPins.vga.get <> masic.vga.get
    }
  }
}
