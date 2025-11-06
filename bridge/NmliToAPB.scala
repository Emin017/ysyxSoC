package bridge.psram

import chisel3._
import chisel3.util._

case class APBParameters(addrBits: Int, dataBits: Int)

class APBBundle(val params: APBParameters) extends Bundle {
  val psel    = Output(Bool())
  val penable = Output(Bool())

  val pwrite = Output(Bool())
  val paddr  = Output(UInt(params.addrBits.W))
  val pwdata = Output(UInt(params.dataBits.W))
  val pstrb  = Output(UInt((params.dataBits / 8).W))

  val pready  = Input(Bool())
  val pslverr = Input(Bool())
  val prdata  = Input(UInt(params.dataBits.W))
}

class NmiIO extends Bundle {
  val valid: Bool = Input(Bool())
  val addr:  UInt = Input(UInt(32.W))
  val wdata: UInt = Input(UInt(32.W))
  val wstrb: UInt = Input(UInt(4.W))
  val rdata: UInt = Output(UInt(32.W))
  val ready: Bool = Output(Bool())
}

class PSRAMQSPIBundle(nss: Int = 4) extends Bundle {
  val spi_sck_o:    Bool = Output(Bool())
  val spi_nss_o:    UInt = Output(UInt(nss.W))
  val spi_io_en_o:  UInt = Output(UInt(4.W))
  val spi_io_in_i:  UInt = Input(UInt(4.W))
  val spi_io_out_o: UInt = Output(UInt(4.W))
  val irq_o:        Bool = Output(Bool())
}

class nmi_psram extends BlackBox {
  val io = IO(new Bundle {
    val clk_i:   Clock           = Input(Clock())
    val rst_n_i: Bool            = Input(Bool())
    val nmi:     NmiIO           = new NmiIO
    val qspi:    PSRAMQSPIBundle = new PSRAMQSPIBundle
  })
}

class PSRAMWrapper(address: BigInt) extends Module {
  val io = IO(new Bundle {
    val in:   APBBundle       = Flipped(new APBBundle(APBParameters(addrBits = 32, dataBits = 32)))
    val qspi: PSRAMQSPIBundle = new PSRAMQSPIBundle
  })
  val npsram: nmi_psram = Module(new nmi_psram)

  val addrReg:  UInt = Reg(UInt(32.W))
  val wdataReg: UInt = Reg(UInt(32.W))
  val wstrbReg: UInt = Reg(UInt(4.W))

  // PSRAM controller has 4 chips, each 8MB (23-bit address + 2-bit chip select).
  // Total addressable space: 32MB (0x0000_0000 ~ 0x01FF_FFFF after offset removal)
  // Map input address range to PSRAM controller's expected format:
  //   - Bits [31:28]: 0x4 (controller identification)
  //   - Bits [27:25]: zero padding
  //   - Bits [24:23]: chip select (handled by psram.sv)
  //   - Bits [22:2]:  address within chip
  //   - Bits [1:0]:   always 0 (word-aligned)
  val addressOffset: UInt = io.in.paddr - address.U
  val chipAndAddr:   UInt = addressOffset(24, 2)  // 23 bits: [24:23] chip select + [22:2] address
  val remappedAddress: UInt = Cat("h4".U(4.W), 0.U(3.W), chipAndAddr, 0.U(2.W))  // 32 bits total

  // Wdata shift according to address(1,0),
  // because nmi psram needs the data that not aligned to be shifted.
  val shiftFlag:  UInt = io.in.paddr(1, 0) & "b11".U(2.W)
  val shiftWdata: UInt = MuxLookup(shiftFlag, 0.U(32.W))(
    Seq(
      "b00".U -> io.in.pwdata,
      "b01".U -> Cat(0.U(8.W), io.in.pwdata(31, 8)),
      "b10".U -> Cat(0.U(16.W), io.in.pwdata(31, 16)),
      "b11".U -> Cat(0.U(24.W), io.in.pwdata(31, 24))
    )
  )

  val psram_idle :: psram_active :: Nil = Enum(2)
  val psram_state: UInt = RegInit(psram_idle)

  val setup: Bool = io.in.psel && !io.in.penable

  switch(psram_state) {
    is(psram_idle) {
      psram_state := Mux(setup, psram_active, psram_idle)
      addrReg     := Mux(setup, remappedAddress, addrReg)
      wdataReg    := Mux(setup, shiftWdata, wdataReg)
      wstrbReg    := Mux(setup, Mux(io.in.pwrite, io.in.pstrb, 0.U), wstrbReg)
    }
    is(psram_active) {
      psram_state := Mux(npsram.io.nmi.ready, psram_idle, psram_active)
    }
  }
  val active: Bool = psram_state === psram_active

  npsram.io.clk_i     := clock
  npsram.io.rst_n_i   := !reset.asBool
  npsram.io.nmi.valid := active
  npsram.io.nmi.addr  := addrReg
  npsram.io.nmi.wdata := wdataReg
  npsram.io.nmi.wstrb := wstrbReg
  io.in.prdata        := npsram.io.nmi.rdata
  io.in.pready        := !active || npsram.io.nmi.ready
  io.in.pslverr       := false.B
  io.qspi <> npsram.io.qspi
}
