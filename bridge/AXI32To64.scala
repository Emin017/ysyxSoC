package bridge.axi

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import chisel3.experimental.dataview.DataView

class AXIBundleAR(addrWidth: Int) extends Bundle {
  val addr:  UInt = UInt(addrWidth.W)
  val id:    UInt = UInt(4.W)
  val len:   UInt = UInt(8.W)
  val size:  UInt = UInt(3.W)
  val burst: UInt = UInt(2.W)
}

class AXIBundleR(dataWidth: Int) extends Bundle {
  val resp: UInt = UInt(2.W)
  val data: UInt = UInt(dataWidth.W)
  val last: Bool = Bool()
  val id:   UInt = UInt(4.W)
}

class AXIBundleAW(addrWidth: Int) extends Bundle {
  val addr:  UInt = UInt(addrWidth.W)
  val id:    UInt = UInt(4.W)
  val len:   UInt = UInt(8.W)
  val size:  UInt = UInt(3.W)
  val burst: UInt = UInt(2.W)
}

class AXIBundleW(dataWidth: Int) extends Bundle {
  val data: UInt = UInt(dataWidth.W)
  val strb: UInt = UInt((dataWidth / 8).W)
  val last: Bool = Bool()
}

class AXIBundleB extends Bundle {
  val resp: UInt = UInt(2.W)
  val id:   UInt = UInt(4.W)
}

class AXIBundle(addrWidth: Int, dataWidth: Int) extends Bundle {
  val ar: IrrevocableIO[AXIBundleAR] = Irrevocable(new AXIBundleAR(addrWidth))
  val r:  IrrevocableIO[AXIBundleR]  = Flipped(Irrevocable(new AXIBundleR(dataWidth)))
  val aw: IrrevocableIO[AXIBundleAW] = Irrevocable(new AXIBundleAW(addrWidth))
  val w:  IrrevocableIO[AXIBundleW]  = Irrevocable(new AXIBundleW(dataWidth))
  val b:  IrrevocableIO[AXIBundleB]  = Flipped(Irrevocable(new AXIBundleB))
}

class VerilogAXIBundle(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val awready: Bool = Input(Bool())
  val awvalid: Bool = Output(Bool())
  val awaddr:  UInt = Output(UInt(addrWidth.W))
  val awid:    UInt = Output(UInt(4.W))
  val awlen:   UInt = Output(UInt(8.W))
  val awsize:  UInt = Output(UInt(3.W))
  val awburst: UInt = Output(UInt(2.W))
  val wready:  Bool = Input(Bool())
  val wvalid:  Bool = Output(Bool())
  val wdata:   UInt = Output(UInt(dataWidth.W))
  val wstrb:   UInt = Output(UInt((dataWidth / 8).W))
  val wlast:   Bool = Output(Bool())
  val bready:  Bool = Output(Bool())
  val bvalid:  Bool = Input(Bool())
  val bresp:   UInt = Input(UInt(2.W))
  val bid:     UInt = Input(UInt(4.W))
  val arready: Bool = Input(Bool())
  val arvalid: Bool = Output(Bool())
  val araddr:  UInt = Output(UInt(addrWidth.W))
  val arid:    UInt = Output(UInt(4.W))
  val arlen:   UInt = Output(UInt(8.W))
  val arsize:  UInt = Output(UInt(3.W))
  val arburst: UInt = Output(UInt(2.W))
  val rready:  Bool = Output(Bool())
  val rvalid:  Bool = Input(Bool())
  val rresp:   UInt = Input(UInt(2.W))
  val rdata:   UInt = Input(UInt(dataWidth.W))
  val rlast:   Bool = Input(Bool())
  val rid:     UInt = Input(UInt(4.W))
}

object AXIBundle {
  implicit val master: DataView[VerilogAXIBundle, AXIBundle] = DataView[VerilogAXIBundle, AXIBundle](
    vab => new AXIBundle(vab.addrWidth, vab.dataWidth),
    _.awvalid -> _.aw.valid,
    _.awready -> _.aw.ready,
    _.awid    -> _.aw.bits.id,
    _.awaddr  -> _.aw.bits.addr,
    _.awlen   -> _.aw.bits.len,
    _.awsize  -> _.aw.bits.size,
    _.awburst -> _.aw.bits.burst,
    _.wready  -> _.w.ready,
    _.wvalid  -> _.w.valid,
    _.wdata   -> _.w.bits.data,
    _.wstrb   -> _.w.bits.strb,
    _.wlast   -> _.w.bits.last,
    _.bready  -> _.b.ready,
    _.bvalid  -> _.b.valid,
    _.bresp   -> _.b.bits.resp,
    _.bid     -> _.b.bits.id,
    _.arready -> _.ar.ready,
    _.arvalid -> _.ar.valid,
    _.araddr  -> _.ar.bits.addr,
    _.arid    -> _.ar.bits.id,
    _.arlen   -> _.ar.bits.len,
    _.arsize  -> _.ar.bits.size,
    _.arburst -> _.ar.bits.burst,
    _.rready  -> _.r.ready,
    _.rvalid  -> _.r.valid,
    _.rresp   -> _.r.bits.resp,
    _.rdata   -> _.r.bits.data,
    _.rlast   -> _.r.bits.last,
    _.rid     -> _.r.bits.id
  )
}

class AXIIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in:  VerilogAXIBundle = Flipped(new VerilogAXIBundle(addrWidth = 32, dataWidth = 32))
  val out: VerilogAXIBundle = new VerilogAXIBundle(addrWidth = 32, dataWidth = 64)
}

class AXI32To64 extends FixedIORawModule[AXIIO](new AXIIO) with ImplicitClock with ImplicitReset {
  override protected def implicitClock: Clock = io.clock

  override protected def implicitReset: Reset = io.reset

  private val in:  AXIBundle = io.in.viewAs[AXIBundle]
  private val out: AXIBundle = io.out.viewAs[AXIBundle]

  out
    .exclude(
      _.ar.bits.addr,
      _.aw.bits.addr,
      _.w.bits.strb,
      _.w.bits.data
    )
    .squeezeAll :<>= in
    .exclude(
      _.ar.bits.addr,
      _.aw.bits.addr,
      _.w.bits.strb,
      _.w.bits.data
    )
    .squeezeAll
  out.ar.bits.addr := in.ar.bits.addr - "h60000000".U(32.W)
  out.aw.bits.addr := in.aw.bits.addr - "h60000000".U(32.W)

  val beatOffset = in.aw.bits.addr(2, 0)
  val wordOffset = Cat(0.U(1.W), in.aw.bits.addr(1, 0))
  val shiftBytes = beatOffset - wordOffset
  val shiftBits  = shiftBytes << 3

  val wstrb    = in.w.bits.strb
  val wstrbExt = Cat(0.U(4.W), wstrb)
  val wdataExt = Cat(0.U(32.W), in.w.bits.data)

  val strbAligened = (wstrbExt << shiftBytes)(7, 0) // Shift left based on address offset
  val dataAligened = (wdataExt << shiftBits)(63, 0) // Shift left based on address offset

  out.w.bits.strb := strbAligened
  out.w.bits.data := dataAligened
}
