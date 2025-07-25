/*
Copyright (c) 2017-2022, The Regents of the University of California
(Regents).  All Rights Reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the Regents nor the
   names of its contributors may be used to endorse or promote products
   derived from this software without specific prior written permission.

IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING
OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF REGENTS HAS
BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
*/

/*
This file is copy-pasted from MIDAS, with adjustments for fpga-shells.
*/

package chipyard.fpga.u55c

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util.ElaborationArtefacts
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.prci.ClockSinkNode
import freechips.rocketchip.prci.ClockSinkParameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.util.BooleanToAugmentedBoolean

/** An AXI4 bundle definition whose names should match the interfaces exposed on Xilinx IP blocks. aresetn and clock are
  * omitted, and no user fields are provided.
  */
class XilinxAXI4Bundle(val bundleParams: AXI4BundleParameters, val isAXI4Lite: Boolean = false, val qos: Boolean = true) extends Bundle {
  //TODO: User fields
  require(bundleParams.echoFields == Nil)
  require(bundleParams.requestFields == Nil)
  require(bundleParams.responseFields == Nil)
  require(!isAXI4Lite || (bundleParams.dataBits == 64 || bundleParams.dataBits == 32))

  def AXI4Only[T <: Data](field: T): Option[T] = if (isAXI4Lite) None else Some(field)
  def axi4LiteSize = log2Ceil(bundleParams.dataBits / 8)

  val awid    = AXI4Only(Output(UInt(bundleParams.idBits.W)))
  val awaddr  = Output(UInt(bundleParams.addrBits.W))
  val awlen   = AXI4Only(Output(UInt(AXI4Parameters.lenBits.W)))
  val awsize  = AXI4Only(Output(UInt(AXI4Parameters.sizeBits.W)))
  val awburst = AXI4Only(Output(UInt(AXI4Parameters.burstBits.W)))
  val awlock  = AXI4Only(Output(UInt(AXI4Parameters.lockBits.W)))
  val awcache = AXI4Only(Output(UInt(AXI4Parameters.cacheBits.W)))
  val awprot  = Output(UInt(AXI4Parameters.protBits.W))

  //val awregion = Output(UInt(AXI4Parameters.regionBits.W))
  val awqos   = qos.option(AXI4Only(Output(UInt(AXI4Parameters.qosBits.W)))).flatten
  val awvalid = Output(Bool())
  val awready = Input(Bool())

  val wdata  = Output(UInt(bundleParams.dataBits.W))
  val wstrb  = Output(UInt((bundleParams.dataBits / 8).W))
  val wlast  = AXI4Only(Output(Bool()))
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  val bid    = AXI4Only(Input(UInt(bundleParams.idBits.W)))
  val bresp  = Input(UInt(AXI4Parameters.respBits.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())

  val arid    = AXI4Only(Output(UInt(bundleParams.idBits.W)))
  val araddr  = Output(UInt(bundleParams.addrBits.W))
  val arlen   = AXI4Only(Output(UInt(AXI4Parameters.lenBits.W)))
  val arsize  = AXI4Only(Output(UInt(AXI4Parameters.sizeBits.W)))
  val arburst = AXI4Only(Output(UInt(AXI4Parameters.burstBits.W)))
  val arlock  = AXI4Only(Output(UInt(AXI4Parameters.lockBits.W)))
  val arcache = AXI4Only(Output(UInt(AXI4Parameters.cacheBits.W)))
  val arprot  = Output(UInt(AXI4Parameters.protBits.W))
  //val arregion = Output(UInt(AXI4Parameters.regionBits.W))
  val arqos   = qos.option(AXI4Only(Output(UInt(AXI4Parameters.qosBits.W)))).flatten
  val arvalid = Output(Bool())
  val arready = Input(Bool())

  val rid    = AXI4Only(Input(UInt(bundleParams.idBits.W)))
  val rdata  = Input(UInt(bundleParams.dataBits.W))
  val rresp  = Input(UInt(AXI4Parameters.respBits.W))
  val rlast  = AXI4Only(Input(Bool()))
  val rvalid = Input(Bool())
  val rready = Output(Bool())

  // TODO: Better name? I really mean rocket-chip type
  def driveStandardAXI4(axi4: AXI4Bundle, axiClock: Clock, axiReset: Bool): Unit = {

    axi4.aw.bits.id    := awid.getOrElse(0.U)
    axi4.aw.bits.addr  := awaddr
    axi4.aw.bits.len   := awlen.getOrElse(0.U)
    axi4.aw.bits.size  := awsize.getOrElse(axi4LiteSize.U)
    axi4.aw.bits.burst := awburst.getOrElse(AXI4Parameters.BURST_INCR)
    axi4.aw.bits.lock  := awlock.getOrElse(0.U)
    axi4.aw.bits.cache := awcache.getOrElse(0.U)
    axi4.aw.bits.prot  := awprot
    axi4.aw.bits.qos   := awqos.getOrElse(0.U)
    axi4.aw.valid      := awvalid
    awready            := axi4.aw.ready

    axi4.w.bits.data := wdata
    axi4.w.bits.strb := wstrb
    axi4.w.bits.last := wlast.getOrElse(true.B)
    axi4.w.valid     := wvalid
    wready           := axi4.w.ready

    bid.foreach { _ := axi4.b.bits.id }
    bresp        := axi4.b.bits.resp
    bvalid       := axi4.b.valid
    axi4.b.ready := bready

    axi4.ar.bits.id    := arid.getOrElse(0.U)
    axi4.ar.bits.addr  := araddr
    axi4.ar.bits.len   := arlen.getOrElse(0.U)
    axi4.ar.bits.size  := arsize.getOrElse(axi4LiteSize.U)
    axi4.ar.bits.burst := arburst.getOrElse(AXI4Parameters.BURST_INCR)
    axi4.ar.bits.lock  := arlock.getOrElse(0.U)
    axi4.ar.bits.cache := arcache.getOrElse(0.U)
    axi4.ar.bits.prot  := arprot
    axi4.ar.bits.qos   := arqos.getOrElse(0.U)
    axi4.ar.valid      := arvalid
    arready            := axi4.ar.ready

    rid.foreach { _ := axi4.r.bits.id }
    rdata        := axi4.r.bits.data
    rresp        := axi4.r.bits.resp
    rlast.foreach { _ := axi4.r.bits.last }
    rvalid       := axi4.r.valid
    axi4.r.ready := rready
    if (isAXI4Lite) {
      withClockAndReset(axiClock, axiReset) {
        assert(!axi4.r.valid || axi4.r.bits.last)
      }
    }
  }

  def drivenByStandardAXI4(axi4: AXI4Bundle, axiClock: Clock, axiReset: Bool): Unit = {
    awid.foreach { _ := axi4.aw.bits.id }
    awaddr        := axi4.aw.bits.addr
    awlen.foreach { _ := axi4.aw.bits.len }
    awsize.foreach { _ := axi4.aw.bits.size }
    awburst.foreach { _ := axi4.aw.bits.burst }
    awlock.foreach { _ := axi4.aw.bits.lock }
    awcache.foreach { _ := axi4.aw.bits.cache }
    awprot        := axi4.aw.bits.prot
    awqos.foreach { _ := axi4.aw.bits.qos }
    awvalid       := axi4.aw.valid
    axi4.aw.ready := awready

    wdata        := axi4.w.bits.data
    wstrb        := axi4.w.bits.strb
    wlast.foreach { _ := axi4.w.bits.last }
    wvalid       := axi4.w.valid
    axi4.w.ready := wready

    axi4.b.bits.id   := bid.getOrElse(0.U)
    axi4.b.bits.resp := bresp
    axi4.b.valid     := bvalid
    bready           := axi4.b.ready

    arid.foreach { _ := axi4.ar.bits.id }
    araddr        := axi4.ar.bits.addr
    arlen.foreach { _ := axi4.ar.bits.len }
    arsize.foreach { _ := axi4.ar.bits.size }
    arburst.foreach { _ := axi4.ar.bits.burst }
    arlock.foreach { _ := axi4.ar.bits.lock }
    arcache.foreach { _ := axi4.ar.bits.cache }
    arprot        := axi4.ar.bits.prot
    arqos.foreach { _ := axi4.ar.bits.qos }
    arvalid       := axi4.ar.valid
    axi4.ar.ready := arready

    axi4.r.bits.id   := rid.getOrElse(0.U)
    axi4.r.bits.data := rdata
    axi4.r.bits.resp := rresp
    axi4.r.bits.last := rlast.getOrElse(true.B)
    axi4.r.valid     := rvalid
    rready           := axi4.r.ready

    if (isAXI4Lite) {
      withClockAndReset(axiClock, axiReset) {
        assert(!axi4.aw.valid || axi4.aw.bits.len === 0.U)
        assert(!axi4.aw.valid || axi4.aw.bits.size === axi4LiteSize.U)
        // Use a diplomatic widget to strip down the ID space to a single value
        assert(!axi4.aw.valid || axi4.aw.bits.id === 0.U)
        assert(!axi4.w.valid || axi4.w.bits.last)
        assert(!axi4.ar.valid || axi4.ar.bits.len === 0.U)
        // Use a diplomatic widget to strip down the ID space to a single value
        assert(!axi4.ar.valid || axi4.ar.bits.id === 0.U)
        assert(!axi4.ar.valid || axi4.ar.bits.size === axi4LiteSize.U)
      }
    }
  }

  def tieoffAsManager(): Unit = {
    awid.foreach { _ := DontCare }
    awaddr  := DontCare
    awlen.foreach { _ := DontCare }
    awsize.foreach { _ := DontCare }
    awburst.foreach { _ := DontCare }
    awlock.foreach { _ := DontCare }
    awcache.foreach { _ := DontCare }
    awprot  := DontCare
    awqos.foreach { _ := DontCare }
    awvalid := false.B

    wdata  := DontCare
    wstrb  := DontCare
    wlast.foreach { _ := DontCare }
    wvalid := false.B

    bready := false.B

    arid.foreach { _ := DontCare }
    araddr  := DontCare
    arlen.foreach { _ := DontCare }
    arsize.foreach { _ := DontCare }
    arburst.foreach { _ := DontCare }
    arlock.foreach { _ := DontCare }
    arcache.foreach { _ := DontCare }
    arprot  := DontCare
    arqos.foreach { _ := DontCare }
    arvalid := false.B

    rready := false.B
  }
}

class XilinxAXI4UpperBundle(val bundleParams: AXI4BundleParameters, val isAXI4Lite: Boolean = false, val qos: Boolean = true) extends Bundle {
  //TODO: User fields
  require(bundleParams.echoFields == Nil)
  require(bundleParams.requestFields == Nil)
  require(bundleParams.responseFields == Nil)
  require(!isAXI4Lite || (bundleParams.dataBits == 64 || bundleParams.dataBits == 32))

  def AXI4Only[T <: Data](field: T): Option[T] = if (isAXI4Lite) None else Some(field)
  def axi4LiteSize = log2Ceil(bundleParams.dataBits / 8)

  val AWID    = AXI4Only(Output(UInt(bundleParams.idBits.W)))
  val AWADDR  = Output(UInt(bundleParams.addrBits.W))
  val AWLEN   = AXI4Only(Output(UInt(AXI4Parameters.lenBits.W)))
  val AWSIZE  = AXI4Only(Output(UInt(AXI4Parameters.sizeBits.W)))
  val AWBURST = AXI4Only(Output(UInt(AXI4Parameters.burstBits.W)))
  // val AWLOCK  = AXI4Only(Output(UInt(AXI4Parameters.lockBits.W)))
  // val AWCACHE = AXI4Only(Output(UInt(AXI4Parameters.cacheBits.W)))
  // val AWPROT  = Output(UInt(AXI4Parameters.protBits.W))

  //val awregion = Output(UInt(AXI4Parameters.regionBits.W))
  // val AWQOS   = qos.option(AXI4Only(Output(UInt(AXI4Parameters.qosBits.W)))).flatten
  val AWVALID = Output(Bool())
  val AWREADY = Input(Bool())

  val WDATA  = Output(UInt(bundleParams.dataBits.W))
  val WSTRB  = Output(UInt((bundleParams.dataBits / 8).W))
  val WLAST  = AXI4Only(Output(Bool()))
  val WVALID = Output(Bool())
  val WREADY = Input(Bool())

  val BID    = AXI4Only(Input(UInt(bundleParams.idBits.W)))
  val BRESP  = Input(UInt(AXI4Parameters.respBits.W))
  val BVALID = Input(Bool())
  val BREADY = Output(Bool())

  val ARID    = AXI4Only(Output(UInt(bundleParams.idBits.W)))
  val ARADDR  = Output(UInt(bundleParams.addrBits.W))
  val ARLEN   = AXI4Only(Output(UInt(AXI4Parameters.lenBits.W)))
  val ARSIZE  = AXI4Only(Output(UInt(AXI4Parameters.sizeBits.W)))
  val ARBURST = AXI4Only(Output(UInt(AXI4Parameters.burstBits.W)))
  // val ARLOCK  = AXI4Only(Output(UInt(AXI4Parameters.lockBits.W)))
  // val ARCACHE = AXI4Only(Output(UInt(AXI4Parameters.cacheBits.W)))
  // val ARPROT  = Output(UInt(AXI4Parameters.protBits.W))
  //val arregion = Output(UInt(AXI4Parameters.regionBits.W))
  // val ARQOS   = qos.option(AXI4Only(Output(UInt(AXI4Parameters.qosBits.W)))).flatten
  val ARVALID = Output(Bool())
  val ARREADY = Input(Bool())

  val RID    = AXI4Only(Input(UInt(bundleParams.idBits.W)))
  val RDATA  = Input(UInt(bundleParams.dataBits.W))
  val RRESP  = Input(UInt(AXI4Parameters.respBits.W))
  val RLAST  = AXI4Only(Input(Bool()))
  val RVALID = Input(Bool())
  val RREADY = Output(Bool())

  // TODO: Better name? I really mean rocket-chip type
  def driveStandardAXI4(axi4: AXI4Bundle, axiClock: Clock, axiReset: Bool): Unit = {

    axi4.aw.bits.id    := AWID.getOrElse(0.U)
    axi4.aw.bits.addr  := AWADDR
    axi4.aw.bits.len   := AWLEN.getOrElse(0.U)
    axi4.aw.bits.size  := AWSIZE.getOrElse(axi4LiteSize.U)
    axi4.aw.bits.burst := AWBURST.getOrElse(AXI4Parameters.BURST_INCR)
    // axi4.aw.bits.lock  := AWLOCK.getOrElse(0.U)
    // axi4.aw.bits.cache := AWCACHE.getOrElse(0.U)
    // axi4.aw.bits.prot  := AWPROT
    // axi4.aw.bits.qos   := AWQOS.getOrElse(0.U)
    axi4.aw.valid      := AWVALID
    AWREADY            := axi4.aw.ready

    axi4.w.bits.data := WDATA
    axi4.w.bits.strb := WSTRB
    axi4.w.bits.last := WLAST.getOrElse(true.B)
    axi4.w.valid     := WVALID
    WREADY           := axi4.w.ready

    BID.foreach { _ := axi4.b.bits.id }
    BRESP        := axi4.b.bits.resp
    BVALID       := axi4.b.valid
    axi4.b.ready := BREADY

    axi4.ar.bits.id    := ARID.getOrElse(0.U)
    axi4.ar.bits.addr  := ARADDR
    axi4.ar.bits.len   := ARLEN.getOrElse(0.U)
    axi4.ar.bits.size  := ARSIZE.getOrElse(axi4LiteSize.U)
    axi4.ar.bits.burst := ARBURST.getOrElse(AXI4Parameters.BURST_INCR)
    // axi4.ar.bits.lock  := ARLOCK.getOrElse(0.U)
    // axi4.ar.bits.cache := ARCACHE.getOrElse(0.U)
    // axi4.ar.bits.prot  := ARPROT
    // axi4.ar.bits.qos   := ARQOS.getOrElse(0.U)
    axi4.ar.valid      := ARVALID
    ARREADY            := axi4.ar.ready

    RID.foreach { _ := axi4.r.bits.id }
    RDATA        := axi4.r.bits.data
    RRESP        := axi4.r.bits.resp
    RLAST.foreach { _ := axi4.r.bits.last }
    RVALID       := axi4.r.valid
    axi4.r.ready := RREADY
    if (isAXI4Lite) {
      withClockAndReset(axiClock, axiReset) {
        assert(!axi4.r.valid || axi4.r.bits.last)
      }
    }
  }

  def drivenByStandardAXI4(axi4: AXI4Bundle, axiClock: Clock, axiReset: Bool): Unit = {
    AWID.foreach { _ := axi4.aw.bits.id }
    AWADDR        := axi4.aw.bits.addr
    AWLEN.foreach { _ := axi4.aw.bits.len }
    AWSIZE.foreach { _ := axi4.aw.bits.size }
    AWBURST.foreach { _ := axi4.aw.bits.burst }
    // AWLOCK.foreach { _ := axi4.aw.bits.lock }
    // AWCACHE.foreach { _ := axi4.aw.bits.cache }
    // AWPROT        := axi4.aw.bits.prot
    // AWQOS.foreach { _ := axi4.aw.bits.qos }
    AWVALID       := axi4.aw.valid
    axi4.aw.ready := AWREADY

    WDATA        := axi4.w.bits.data
    WSTRB        := axi4.w.bits.strb
    WLAST.foreach { _ := axi4.w.bits.last }
    WVALID       := axi4.w.valid
    axi4.w.ready := WREADY

    axi4.b.bits.id   := BID.getOrElse(0.U)
    axi4.b.bits.resp := BRESP
    axi4.b.valid     := BVALID
    BREADY           := axi4.b.ready

    ARID.foreach { _ := axi4.ar.bits.id }
    ARADDR        := axi4.ar.bits.addr
    ARLEN.foreach { _ := axi4.ar.bits.len }
    ARSIZE.foreach { _ := axi4.ar.bits.size }
    ARBURST.foreach { _ := axi4.ar.bits.burst }
    // ARLOCK.foreach { _ := axi4.ar.bits.lock }
    // ARCACHE.foreach { _ := axi4.ar.bits.cache }
    // ARPROT        := axi4.ar.bits.prot
    // ARQOS.foreach { _ := axi4.ar.bits.qos }
    ARVALID       := axi4.ar.valid
    axi4.ar.ready := ARREADY

    axi4.r.bits.id   := RID.getOrElse(0.U)
    axi4.r.bits.data := RDATA
    axi4.r.bits.resp := RRESP
    axi4.r.bits.last := RLAST.getOrElse(true.B)
    axi4.r.valid     := RVALID
    RREADY           := axi4.r.ready

    if (isAXI4Lite) {
      withClockAndReset(axiClock, axiReset) {
        assert(!axi4.aw.valid || axi4.aw.bits.len === 0.U)
        assert(!axi4.aw.valid || axi4.aw.bits.size === axi4LiteSize.U)
        // Use a diplomatic widget to strip down the ID space to a single value
        assert(!axi4.aw.valid || axi4.aw.bits.id === 0.U)
        assert(!axi4.w.valid || axi4.w.bits.last)
        assert(!axi4.ar.valid || axi4.ar.bits.len === 0.U)
        // Use a diplomatic widget to strip down the ID space to a single value
        assert(!axi4.ar.valid || axi4.ar.bits.id === 0.U)
        assert(!axi4.ar.valid || axi4.ar.bits.size === axi4LiteSize.U)
      }
    }
  }

  def tieoffAsManager(): Unit = {
    AWID.foreach { _ := DontCare }
    AWADDR  := DontCare
    AWLEN.foreach { _ := DontCare }
    AWSIZE.foreach { _ := DontCare }
    AWBURST.foreach { _ := DontCare }
    // AWLOCK.foreach { _ := DontCare }
    // AWCACHE.foreach { _ := DontCare }
    // AWPROT  := DontCare
    // AWQOS.foreach { _ := DontCare }
    AWVALID := false.B

    WDATA  := DontCare
    WSTRB  := DontCare
    WLAST.foreach { _ := DontCare }
    WVALID := false.B

    BREADY := false.B

    ARID.foreach { _ := DontCare }
    ARADDR  := DontCare
    ARLEN.foreach { _ := DontCare }
    ARSIZE.foreach { _ := DontCare }
    ARBURST.foreach { _ := DontCare }
    // ARLOCK.foreach { _ := DontCare }
    // ARCACHE.foreach { _ := DontCare }
    // ARPROT  := DontCare
    // ARQOS.foreach { _ := DontCare }
    ARVALID := false.B

    RREADY := false.B
  }
}

class AXI4ClockConverter(
  bundleParams:             AXI4BundleParameters,
  override val desiredName: String,
  isAXI4Lite:               Boolean = false,
) extends BlackBox {
  val io = IO(new Bundle {
    val s_axi_aclk    = Input(Clock())
    val s_axi_aresetn = Input(AsyncReset())
    val s_axi         = Flipped(new XilinxAXI4Bundle(bundleParams, isAXI4Lite))

    val m_axi_aclk    = Input(Clock())
    val m_axi_aresetn = Input(AsyncReset())
    val m_axi         = new XilinxAXI4Bundle(bundleParams, isAXI4Lite)
  })

  val protocolParam = if (isAXI4Lite) "AXI4LITE" else "AXI4"

  ElaborationArtefacts.add(s"$desiredName.vivado.tcl",
    s"""|create_ip -name axi_clock_converter \\
        |          -vendor xilinx.com \\
        |          -library ip \\
        |          -version 2.1 \\
        |          -module_name $desiredName
        |
        |set_property -dict [list CONFIG.PROTOCOL {$protocolParam} \\
        |                         CONFIG.ADDR_WIDTH {${bundleParams.addrBits}} \\
        |                         CONFIG.SYNCHRONIZATION_STAGES {3} \\
        |                         CONFIG.DATA_WIDTH {${bundleParams.dataBits}} \\
        |                         CONFIG.ID_WIDTH {${bundleParams.idBits}} \\
        |                         CONFIG.AWUSER_WIDTH {0} \\
        |                         CONFIG.ARUSER_WIDTH {0} \\
        |                         CONFIG.RUSER_WIDTH {0} \\
        |                         CONFIG.WUSER_WIDTH {0} \\
        |                         CONFIG.BUSER_WIDTH {0}] \\
        |             [get_ips $desiredName]
        |""".stripMargin
  )
}


// Make AXI4ClockConverter diplomatic.
class LazyAXI4ClockConverter(moduleNamePrefix: String, isAXILite: Boolean)(implicit p: Parameters) extends LazyModule {

  val node = AXI4IdentityNode()
  val slaveClockNode = ClockSinkNode(Seq(ClockSinkParameters()))
  val masterClockNode = ClockSinkNode(Seq(ClockSinkParameters()))

  lazy val module = new LazyRawModuleImp(this) {
    val slaveClock = slaveClockNode.in.head._1.clock
    val slaveReset = slaveClockNode.in.head._1.reset
    val masterClock = masterClockNode.in.head._1.clock
    val masterReset = masterClockNode.in.head._1.reset

    node.in.zip(node.out).zipWithIndex.foreach { case (((in, edge), (out, _)), i) =>
      val converter = Module(new AXI4ClockConverter(
        bundleParams = edge.bundle,
        desiredName = s"${moduleNamePrefix}_axi_clock_converter_$i",
        isAXI4Lite = isAXILite,
      ))

      converter.io.s_axi_aclk := slaveClock
      converter.io.s_axi_aresetn := (!slaveReset.asBool).asAsyncReset

      converter.io.m_axi_aclk := masterClock
      converter.io.m_axi_aresetn := (!masterReset.asBool).asAsyncReset

      converter.io.s_axi.drivenByStandardAXI4(in, slaveClock, slaveReset.asBool)
      converter.io.m_axi.driveStandardAXI4(out, masterClock, masterReset.asBool)
    }
  }
}