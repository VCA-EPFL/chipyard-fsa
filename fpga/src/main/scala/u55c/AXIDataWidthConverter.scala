package chipyard.fpga.u55c

import chisel3._
import chisel3.experimental.dataview._
import chisel3.reflect.DataMirror
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.{AddressSet, RegionType, TransferSizes}
import freechips.rocketchip.util.ElaborationArtefacts
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.prci.ClockSinkNode
import freechips.rocketchip.prci.ClockSinkParameters
import freechips.rocketchip.resources.SimpleDevice
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.util.BooleanToAugmentedBoolean
import fsa.arithmetic.FloatPoint

import scala.collection.immutable.SeqMap

class XilinxAXIDataWidthConverterIO(sParams: AXI4BundleParameters, mParams: AXI4BundleParameters) extends Bundle {
  val s_axi = Flipped(new XilinxAXI4Bundle(mParams))
  val m_axi = new XilinxAXI4Bundle(sParams)
  val s_axi_aclk = Input(Clock())
  val s_axi_aresetn = Input(Reset())
}

class XilinxAXIDataWidthConverter
(
  slaveParams: AXI4BundleParameters,
  masterParams: AXI4BundleParameters,
  maxSplitBeats: Int = 16,
  override val desiredName: String
) extends BlackBox {
  val io = IO(new XilinxAXIDataWidthConverterIO(slaveParams, masterParams))

  val masterDataWidth = masterParams.dataBits
  val slaveDataWidth = slaveParams.dataBits
  val slaveIDWidth = slaveParams.idBits

  println("Debug master", masterDataWidth)
  println("Debug slave", slaveDataWidth)

  // always
  val slaveAddrWidth = slaveParams.addrBits

  def isPow2(n: Int) = {
    n > 0 && (n & (n - 1)) == 0
  }

  require(masterDataWidth != slaveDataWidth)
  require(isPow2(masterDataWidth) && isPow2(slaveDataWidth))
  require(masterDataWidth <= 1024 && slaveDataWidth <= 1024)
  require(slaveIDWidth <= 32)
  require(maxSplitBeats == 16 || maxSplitBeats == 256) // FIXME: correct burst length determine
  ElaborationArtefacts.add(s"$desiredName.vivado.tcl",
    s"""
    |create_ip -name axi_dwidth_converter -vendor xilinx.com -library ip -version 2.1 -module_name ${desiredName}
    |set_property -dict [list \\
    |  CONFIG.ADDR_WIDTH {33} \\
    |  CONFIG.MAX_SPLIT_BEATS {16} \\
    |  CONFIG.MI_DATA_WIDTH {${slaveDataWidth}} \\
    |  CONFIG.SI_DATA_WIDTH {${masterDataWidth}} \\
    |  CONFIG.SI_ID_WIDTH {5} \\
    |] [get_ips ${desiredName}]
    """.stripMargin)

}

// FIXME: DataWidthConverter does not have id fields
class LazyXilinxAXIDataWidthConverter(moduleNamePrefix: String, beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  def beatBytesUtil(param: AXI4SlavePortParameters) = {
    param.copy(beatBytes = beatBytes)
  }

  val node = AXI4AdapterNode(slaveFn = beatBytesUtil)
  val slaveClockNode = ClockSinkNode(Seq(ClockSinkParameters()))

  lazy val module = new LazyRawModuleImp(this) {
    val in = node.in.head._1
    val out = node.out.head._1
    val slaveClock = slaveClockNode.in.head._1.clock
    val slaveReset = slaveClockNode.in.head._1.reset

    require(node.in.size == 1)
    require(node.out.size == 1)

    val converter = Module(new XilinxAXIDataWidthConverter(
      slaveParams = node.out.head._2.bundle,
      masterParams = node.in.head._2.bundle,
      desiredName = moduleNamePrefix + "_axi_data_width_converter"
    ))

    converter.io.s_axi_aresetn := (!slaveReset.asBool).asAsyncReset
    converter.io.s_axi_aclk := slaveClock
    converter.io.s_axi.drivenByStandardAXI4(in, slaveClock, slaveReset.asBool)
    converter.io.m_axi.driveStandardAXI4(out, slaveClock, slaveReset.asBool)
  }
}
