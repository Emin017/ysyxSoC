package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters)
  extends APB4DevTemplate(address, new SPIIO, hasClockNode = true)((in: APBBundle, outer: LazyModuleImp, irq_o: Bool, extra, genClock, genReset) => {
  val mspi = Module(new spi_top_apb)
  val spiClock = genClock.getOrElse(outer.clock)
  val spiReset = genReset.getOrElse(outer.reset)
  mspi.io.clock := spiClock
  mspi.io.reset := spiReset
  //mspi.io.in <> in
  extra <> mspi.io.spi

  withClockAndReset(spiClock, spiReset) {
    val cmd_idle :: cmd_spi_csr :: cmd_wr_txd0 :: cmd_wr_txd1 :: cmd_wr_div :: cmd_wr_ss :: cmd_wr_ctrl :: cmd_wait_irq :: cmd_rd_rxd0 :: Nil = Enum(9)
    val spi_idle :: spi_enable :: spi_wait_ready :: Nil = Enum(3)
    val cmd_state = RegInit(cmd_idle)
    val spi_state = RegInit(spi_idle)
    val spi_ack = spi_state === spi_wait_ready && mspi.io.in.pready

    val is_flash = in.paddr >= "h3000_0000".U && in.paddr <= "h3fff_ffff".U
    switch (cmd_state) {
      is (cmd_idle) { when (in.psel && in.penable) {
        cmd_state := Mux(is_flash && !in.pwrite, cmd_wr_txd0, cmd_spi_csr)
      } }
      is (cmd_spi_csr) { when (spi_ack) { cmd_state := cmd_idle    } }
      is (cmd_wr_txd0) { when (spi_ack) { cmd_state := cmd_wr_txd1 } }
      is (cmd_wr_txd1) { when (spi_ack) { cmd_state := cmd_wr_div  } }
      is (cmd_wr_div ) { when (spi_ack) { cmd_state := cmd_wr_ss   } }
      is (cmd_wr_ss  ) { when (spi_ack) { cmd_state := cmd_wr_ctrl } }
      is (cmd_wr_ctrl) { when (spi_ack) { cmd_state := cmd_wait_irq} }
      is (cmd_wait_irq){ when (mspi.io.spi_irq_out) { cmd_state := cmd_rd_rxd0 } }
      is (cmd_rd_rxd0) { when (spi_ack) { cmd_state := cmd_idle    } }
    }

    val spi_req = cmd_state =/= cmd_idle && cmd_state =/= cmd_wait_irq
    switch (spi_state) {
      is (spi_idle)       { when (spi_req) { spi_state := spi_enable } }
      is (spi_enable)     { spi_state := spi_wait_ready }
      is (spi_wait_ready) { when (mspi.io.in.pready) { spi_state := spi_idle } }
    }

    val spi_prdata_bswap = Cat(mspi.io.in.prdata.asTypeOf(Vec(4, UInt(8.W))))
    in.prdata := Mux(cmd_state === cmd_rd_rxd0, spi_prdata_bswap, mspi.io.in.prdata)
    in.pready := (cmd_state === cmd_spi_csr || cmd_state === cmd_rd_rxd0) && spi_ack
    mspi.io.in.paddr := 0.U(27.W) ## MuxLookup(cmd_state, in.paddr(4, 2))(Array(
      cmd_wr_txd0  -> 0.U(3.W), cmd_wr_txd1  -> 1.U(3.W), cmd_wr_div   -> 5.U(3.W),
      cmd_wr_ss    -> 6.U(3.W), cmd_wr_ctrl  -> 4.U(3.W), cmd_rd_rxd0  -> 0.U(3.W),
    )) ## 0.U(2.W)
    mspi.io.in.psel := spi_state === spi_wait_ready
    mspi.io.in.penable := spi_state === spi_enable || spi_state === spi_wait_ready
    mspi.io.in.pprot := in.pprot
    mspi.io.in.pwrite := Mux(cmd_state === cmd_spi_csr, in.pwrite, (cmd_state =/= cmd_rd_rxd0))
    mspi.io.in.pstrb := Mux(cmd_state === cmd_spi_csr, in.pstrb, "b1111".U)
    mspi.io.in.pwdata := MuxLookup(cmd_state, in.pwdata)(Array(
      cmd_wr_txd0  -> 0.U(32.W), // padding for 32 bit rdata from flash
      cmd_wr_txd1  -> "h03".U(8.W) ## in.paddr(23, 2) ## 0.U(2.W), // flash address and read cmd
      cmd_wr_div   -> 0.U(32.W), // divide by 2
      cmd_wr_ss    -> 1.U(32.W),
      //     ASS         IE      TX NEGEDGE      GO  64-bit transfer
      cmd_wr_ctrl  -> ((1 << 13) | (1 << 12) | (1 << 10) | (1 << 8) | 0x40).U(32.W),
    ))
  }
})
