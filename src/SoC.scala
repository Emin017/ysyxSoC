package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog
import device.ChipLinkWrapper
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.prci.{ClockSourceNode, ClockSourceParameters, FixedClockBroadcast}
import freechips.rocketchip.system.SimAXIMem
import org.chipsalliance.diplomacy.lazymodule.LazyModule

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
  val oldIPClockSource = ClockSourceNode(Seq(ClockSourceParameters(name = Some("old_ip_clk"))))
  // connect old IP clock/reset to external pins
  // These clock/reset signals should be connected to the RCU outputs to align with the previous SoC design.
  private val oldIPClockBroadcast = FixedClockBroadcast(None)
  oldIPClockBroadcast := oldIPClockSource

  val xbar = AXI4Xbar()
  val apbxbar = LazyModule(new APBFanout).node
  val cpu = LazyModule(new CPU(idBits = ChipLinkParam.idBits))
  //val chipMaster = if (Config.hasChipLink) Some(LazyModule(new ChipLinkMaster)) else None
  val chipMaster = if (Config.hasChipLink) Some(LazyModule(new ChipLinkWrapper)) else None
  val chiplinkNode = if (Config.hasChipLink) Some(AXI4SlaveNodeGenerator(p(ExtBus), ChipLinkParam.allSpace)) else None

  def AddrSpace(base: BigInt, len: BigInt = 0x1000) = AddressSet.misaligned(base, len)

  // RISC-V system
  val lclint    = LazyModule(new APB4CLINT   (AddrSpace(0x02010000, 0x10000)))
  val lplic     = LazyModule(new APB4PLIC    (AddrSpace(0x0c000000, 0x40)))

  // generic system
  val luart0    = LazyModule(new APBUart16550(AddrSpace(0x10000000, 0x8)))
  val lspi      = LazyModule(new APBSPI      (AddrSpace(0x10001000, 0x20)   ++     // SPI controller
                                              AddrSpace(0x30000000, 0x10000000)))  // XIP flash
  val lrtc      = LazyModule(new APB4RTC     (AddrSpace(0x10004000, 0x20)))
  val lwdg      = LazyModule(new APB4WDG     (AddrSpace(0x10005000, 0x20)))
  val larchinfo = LazyModule(new APB4ArchInfo(AddrSpace(0x10006000, 0x10)))

  val lrcu      = LazyModule(new APB4RCU     (AddrSpace(0x10002000, 0x1000)))

  // interface
  val lgpio0    = LazyModule(new APB4GPIO    (AddrSpace(0x10100000, 0x40)))
  val lgpio1    = LazyModule(new APB4GPIO    (AddrSpace(0x10101000, 0x40)))
  val lgpio2    = LazyModule(new APB4GPIO    (AddrSpace(0x10102000, 0x40)))
  val luart1    = LazyModule(new APB4UART    (AddrSpace(0x10103000, 0x20)))
  val li2c      = LazyModule(new APB4I2C     (AddrSpace(0x10104000, 0x20)))
  val lps2      = LazyModule(new APB4PS2     (AddrSpace(0x10105000, 0x10)))
  val lpwm0     = LazyModule(new APB4PWM     (AddrSpace(0x10106000, 0x40)))
  val lpwm1     = LazyModule(new APB4PWM     (AddrSpace(0x10107000, 0x40)))
  val ltim0     = LazyModule(new APB4Timer   (AddrSpace(0x10108000, 0x20)))
  val ltim1     = LazyModule(new APB4Timer   (AddrSpace(0x10109000, 0x20)))
  val ltim2     = LazyModule(new APB4Timer   (AddrSpace(0x1010a000, 0x20)))
  val ltim3     = LazyModule(new APB4Timer   (AddrSpace(0x1010b000, 0x20)))

  // multimedia
  val lqspi     = LazyModule(new APB4QSPI    (AddrSpace(0x10200000, 0x20)))
  val li2s      = LazyModule(new APB4I2S     (AddrSpace(0x10201000, 0x20)))

  // application
  val lrng      = LazyModule(new APB4RNG     (AddrSpace(0x10300000, 0x10)))
  val lcrc      = LazyModule(new APB4CRC     (AddrSpace(0x10301000, 0x20)))

  // memory
  val sdramAddressSet = AddrSpace(0x80000000L, 0x2000000)
  val lsdram_apb = if (!Config.sdramUseAXI) Some(LazyModule(new APBSDRAM (sdramAddressSet))) else None
  val lsdram_axi = if ( Config.sdramUseAXI) Some(LazyModule(new AXI4SDRAM(sdramAddressSet))) else None

  // homework
  val lgpio     = if (Config.hasHomeWork) Some(LazyModule(new APBGPIO    (AddrSpace(0x10002000, 0x10)))) else None
  val lkeyboard = if (Config.hasHomeWork) Some(LazyModule(new APBKeyboard(AddrSpace(0x10011000, 0x8)))) else None
  //val lvga      = Some(LazyModule(new APBVGA     (AddrSpace(0x21000000, 0x200000))))

  // val lpsram = LazyModule(new APBPSRAM(AddrSpace(0xa0000000L, 0x400000)))
  val lpsram = LazyModule(new APBPSRAM(AddrSpace(0xC0000000L, 0x2000000)))

  List(lclint, lplic,
       lspi, luart0, lrtc, lwdg, larchinfo,
       lgpio0, lgpio1, lgpio2, luart1, li2c, lps2, lpwm0, lpwm1, ltim0, ltim1, ltim2, ltim3,
       lqspi, li2s,
       lrng, lcrc,
       lpsram,
       lrcu
  ).map(_.node := apbxbar)

  if (Config.isDstage) {
    apbxbar := APBDelayer() := AXI4ToAPB() := AXI4Buffer() := xbar
  } else if (Config.hasHomeWork) {
    val xbar2 = AXI4Xbar()
    // List(lpsram, lgpio, lkeyboard, lvga).map(_.get.node := apbxbar)
    List(lgpio, lkeyboard).map(_.get.node := apbxbar)
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
  if (Config.hasChipLink) chipMaster.get.clockNode := oldIPClockBroadcast
  xbar := cpu.masterNode

  luart0.clockNode.get := oldIPClockBroadcast
  lspi.clockNode.get   := oldIPClockBroadcast

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

    val oldIPClock = IO(Input(Clock()))
    val oldIPReset = IO(Input(Reset()))
    // connect old IP clock/reset to external pins
    // These clock/reset signals should be connected to the RCU outputs to align with the previous SoC design.
    oldIPClockSource.out.head._1.clock := oldIPClock
    oldIPClockSource.out.head._1.reset := oldIPReset

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
    lrtc.module.extra.rtc_clk_i := clock_half
    lrtc.module.extra.rtc_rst_n_i := !reset.asBool
    lwdg.module.extra.rtc_clk_i := clock_half

//    val i2c_io = li2c.module.extra
//    val i2c_scl = IO(Analog(1.W))
//    val i2c_sda = IO(Analog(1.W))
//    i2c_io.scl_i := TriStateInBuf(i2c_scl, i2c_io.scl_o, i2c_io.scl_dir_o)
//    i2c_io.sda_i := TriStateInBuf(i2c_sda, i2c_io.sda_o, i2c_io.sda_dir_o)

    val i2s_io = li2s.module.extra
    val i2s_sck = IO(Analog(1.W))
    val i2s_ws  = IO(Analog(1.W))
    i2s_io.sck_i := TriStateInBuf(i2s_sck, i2s_io.sck_o, i2s_io.sck_en_o)
    i2s_io.ws_i  := TriStateInBuf(i2s_ws, i2s_io.ws_o, i2s_io.ws_en_o)
    i2s_io.sd_i := false.B

    val qspi_io = lqspi.module.extra
    val qspi_sck_o = IO(Output(Bool()))
    val qspi_nss_o = IO(Output(UInt(4.W)))
    val qspi_en_o = IO(Output(UInt(4.W)))
    val qspi_out_o = IO(Output(UInt(4.W)))
    val qspi_in_i = IO(Input(UInt(4.W)))
    qspi_sck_o := qspi_io.spi_sck_o
    qspi_nss_o := qspi_io.spi_nss_o
    qspi_en_o := qspi_io.spi_io_en_o
    qspi_out_o := qspi_io.spi_io_out_o
    qspi_io.spi_io_in_i := qspi_in_i
    //val qspi_dio  = IO(Vec(4, Analog(1.W)))
    //qspi_io.spi_io_in_i := Cat((0 to 3).map(i => TriStateInBuf(qspi_dio(i), qspi_io.spi_io_out_o(i), qspi_io.spi_io_en_o(i))).reverse)

    val psram_io = lpsram.module.extra
    val psram_sck = IO(Output(Bool()))
    val psram_nss = IO(Output(UInt(2.W)))
    val psram_en_o = IO(Output(UInt(4.W)))
    val psram_in_i = IO(Input(UInt(4.W)))
    val psram_out_o = IO(Output(UInt(4.W)))
    psram_sck := psram_io.spi_sck_o
    psram_nss :<= psram_io.spi_nss_o.squeeze
    psram_en_o := psram_io.spi_io_en_o
    psram_out_o := psram_io.spi_io_out_o
    psram_io.spi_io_in_i := psram_in_i

    // connect interrupt signal
    val intr_from_chipSlave = IO(Input(Bool()))
    cpu.module.io_interrupt := lplic.module.irq_o
    lplic.module.extra.irq_i := Cat(List(lgpio0, lgpio1, lgpio2, lrtc, li2c, lqspi, li2s,
      lpwm0, lpwm1, ltim0, ltim1, ltim2, ltim3, lps2).map(_.module.irq_o)) ## intr_from_chipSlave

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
      Some(genAPB4DevIO(name, lmodule.get))
    }

    val uart0 = genAPB4DevIO("uart0", luart0)
    val uart1 = genAPB4DevIO("uart1", luart1)
    val spi   = genAPB4DevIO("spi", lspi)
    val sdram = genIO("sdram", sdramBundle)
    //val gpio  = genSomeAPB4DevIO("gpio", lgpio)
    val ps2   = genAPB4DevIO("ps2", lps2)
    //val vga   = genSomeAPB4DevIO("vga", lvga)

    val gpio0 = genAPB4DevIO("gpio0", lgpio0)
    val gpio1 = genAPB4DevIO("gpio1_i", lgpio1)
    val gpio2 = genAPB4DevIO("gpio2_i", lgpio2)

    val pwm0 = genAPB4DevIO("pwm0", lpwm0)
    val pwm1 = genAPB4DevIO("pwm1", lpwm1)

    val i2c = genAPB4DevIO("i2c", li2c)
    val i2s = genAPB4DevIO("i2s", li2s)

    val timer0 = genAPB4DevIO("timer0", ltim0)
    val timer1 = genAPB4DevIO("timer1", ltim1)
    val timer2 = genAPB4DevIO("timer2", ltim2)
    val timer3 = genAPB4DevIO("timer3", ltim3)

    val rcu    = genAPB4DevIO("rcu", lrcu)

    val core_sel = genIO("core_sel", cpu.module.core_sel)
    val core_irq = genIO("core_irq", cpu.module.io_interrupt)
  }
}

class ysyxSoCFPGA(implicit p: Parameters) extends ChipLinkSlave


class ysyxSoCFull(implicit p: Parameters) extends LazyModule {
  val asic = LazyModule(new ysyxSoCASIC)
  ElaborationArtefacts.add("graphml", graphML)

  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with DontTouch {
    val masic = asic.module
    masic.dontTouchPorts()

    if (Config.hasChipLink) {
      if (Config.enableSimulation) {
        val fpga = LazyModule(new ysyxSoCFPGA)
        val mfpga = Module(fpga.module)

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
      } else {
        masic.fpga_io.get.b2c <> DontCare
      }
    }

    // slower clock
    val divReg = RegInit(false.B)
    divReg := !divReg
    masic.clock_half := divReg

    masic.intr_from_chipSlave := false.B

    masic.ps2.ps2_clk_i := false.B
    masic.ps2.ps2_dat_i := false.B

    masic.oldIPClock := DontCare
    masic.oldIPReset := DontCare

    if (Config.enableSimulation) {
      val gpio_led = Module(new gpio_led_model)
      gpio_led.io.led_i := masic.gpio0.gpio_out_o

      val flash = Module(new flash)
      flash.io <> masic.spi
      flash.io.ss := masic.spi.ss(0)

      val bitrev = Module(new bitrev)
      bitrev.io <> masic.spi
      bitrev.io.ss := masic.spi.ss(7)
      bitrev.io.ss := masic.spi.ss(1)
      masic.spi.miso := List(bitrev.io, flash.io).map(_.miso).reduce(_&&_)

      // val sdram = Module(new sdramChisel)
      // sdram.io <> masic.sdram

      val psramModel = Module(new ESPWrapper)
      psramModel.io <> masic.psram_io

    } else {
      masic.psram_in_i := DontCare
      masic.qspi_in_i := DontCare
      masic.sdram.data_i := DontCare
    }

    val externalPins = IO(new Bundle{
      val uart0 = chiselTypeOf(masic.uart0)
      val uart1 = chiselTypeOf(masic.uart1)

      val gpio0 = chiselTypeOf(masic.gpio0)
      val gpio1 = chiselTypeOf(masic.gpio1)
      val gpio2 = chiselTypeOf(masic.gpio2)

      val spi  = chiselTypeOf(masic.spi)
      //val vga = Some(chiselTypeOf(masic.vga.get))
      val core_sel = chiselTypeOf(masic.core_sel)
      val core_irq = chiselTypeOf(masic.core_irq)

      val pwm0 = chiselTypeOf(masic.pwm0)
      val pwm1 = chiselTypeOf(masic.pwm1)

      val i2c = chiselTypeOf(masic.i2c)
      val timer0 = chiselTypeOf(masic.timer0)
      val timer1 = chiselTypeOf(masic.timer1)
      val timer2 = chiselTypeOf(masic.timer2)
      val timer3 = chiselTypeOf(masic.timer3)

      val ps2 = chiselTypeOf(masic.ps2)
      val i2s = chiselTypeOf(masic.i2s)

      val rcu = chiselTypeOf(masic.rcu)
    })
    externalPins.uart0 <> masic.uart0
    externalPins.uart1 <> masic.uart1

    externalPins.gpio0 <> masic.gpio0
    externalPins.gpio1 <> masic.gpio1
    externalPins.gpio2 <> masic.gpio2

    externalPins.spi   <> masic.spi

    externalPins.core_sel <> masic.core_sel
    externalPins.core_irq <> masic.core_irq

    externalPins.pwm0 <> masic.pwm0
    externalPins.pwm1 <> masic.pwm1

    externalPins.i2c <> masic.i2c

    externalPins.timer0 <> masic.timer0
    externalPins.timer1 <> masic.timer1
    externalPins.timer2 <> masic.timer2
    externalPins.timer3 <> masic.timer3

    externalPins.ps2 <> masic.ps2

    //externalPins.vga.get <> masic.vga.get
    externalPins.i2s <> masic.i2s

    externalPins.rcu <> masic.rcu

  }
}
