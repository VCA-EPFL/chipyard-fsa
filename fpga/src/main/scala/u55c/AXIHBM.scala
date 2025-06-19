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
import msaga.arithmetic.FloatPoint

import scala.collection.immutable.SeqMap

/*
class XilinxChiselHBMIO(val params: Seq[AXI4BundleParameters], val is16GB: Boolean) extends Bundle {
  val AXI_ACLK = Input(Vec(params.size, Clock()))
  val AXI_ARRESETN = Input(Vec(params.size, AsyncReset()))
  val HBM_REF_CLK_0 = Input(Clock())
  val HBM_REF_CLK_1 = if (is16GB) Some(Input(Clock())) else None
  val AXI = Flipped(MixedVec(params.map(p => new XilinxAXI4Bundle(p))))
}
 */



class XilinxHBMIO(val params: Seq[AXI4BundleParameters], val is16GB: Boolean) extends Bundle {
  val AXI_00_ACLK = Input(Clock())
  val AXI_00_ARESET_N = Input(Reset())
  val HBM_REF_CLK_0 = Input(Clock())
  val HBM_REF_CLK_1 = if (is16GB) Some(Input(Clock())) else None
  val AXI_00 = Flipped(new XilinxAXI4UpperBundle(params(0), isAXI4Lite = false))
  val APB_0_PCLK = Input(Clock())
  val APB_1_PCLK = Input(Clock())
  val APB_0_PRESET_N = Input(Reset())
  val APB_1_PRESET_N = Input(Reset())

  /*
  override val elements: SeqMap[String, Data] = SeqMap.from(

    (
      axis.getElements.zipWithIndex.toSeq.flatMap {
        case ((axiBundle), idx) =>
          val p1 = axiBundle.asInstanceOf[XilinxAXI4Bundle].elements.toSeq.map {
            case (axiFieldName, uintData) =>
              s"AXI_${idx % 02d}_${axiFieldName.toUpperCase}" -> uintData
          }
          val p2 = Seq(
            s"AXI_${idx % 02d}_ACLK" -> axi_aclks(idx),
            s"AXI_${idx % 02d}_ARRESETN" -> axi_aresetns(idx)
          )
          p1 ++ p2
      }
        ++
        Seq("HBM_REF_CLK_0" -> HBM_REF_CLK_0) ++
        HBM_REF_CLK_1.map(clk => "HBM_REF_CLK_1" -> clk)
      ).map { case (k, v) => k -> Input(v) }
  )
   */

}
/*
object XilinxHBM {

  private val mappingSeq: Seq[(XilinxChiselHBMIO, XilinxHBMIO) => (Data, Data)] = {
    val chiselNames = new XilinxChiselHBMIO(
      Seq.fill(32)(AXI4BundleParameters.emptyBundleParams),
      is16GB = true
    ).elements.keys
    chiselNames.map(cn => {


      (chiselBundle: XilinxChiselHBMIO, verilogBundle: XilinxHBMIO) => {
        case "AXI_ACLK" | "AXI_ARRESETN" =>
          chiselBundle.elements(cn).asInstanceOf[Vec[Data]].zipWithIndex.map {
            case (elm, idx) => elm -> verilogBundle.elements(s"AXI_${idx % 02d}_${cn.drop(4)}")
          }
        case "AXI" =>
          chiselBundle.elements(cn).asInstanceOf[Vec[XilinxAXI4Bundle]].zipWithIndex.flatMap {
            case (axi, idx) => {
              axi.elements.map {
                case (filed, elm) =>
                  val verilogName = s"AXI_${idx % 02d}_${filed.toUpperCase}"
                  verilogBundle.elements.contains(verilogName).option(
                    elm -> verilogBundle.elements(verilogName)
                  )
              }.flatten
            }
          }
        case _ =>
          verilogBundle.elements.contains(cn).option(
            chiselBundle.elements(cn) -> verilogBundle.elements(cn)
          )
      }

    })
  }


  implicit val hbmView: DataView[XilinxChiselHBMIO, XilinxHBMIO] = DataView(
    x => new XilinxHBMIO(x.params, x.is16GB), mappingSeq: _*
  )

  implicit val hbmView2: DataView[XilinxHBMIO, XilinxChiselHBMIO] = hbmView.invert(
    x => new XilinxChiselHBMIO(x.params, x.is16GB)
  )
}
 */

/*
set_property -dict [list \\
${(0 until 32).map { i => f"CONFIG.USER_SAXI_${i}%02d {${if (i < portNum) "true" else "false"}}" }.mkString(" \\\n")} ] \\
  CONFIG.USER_MC0_ECC_BYPASS {true} \\
  CONFIG.USER_XSDB_INTF_EN {FALSE} \\
  CONFIG.USER_CLK_SEL_LIST0 {AXI_00_ACLK} \\
  CONFIG.USER_HBM_CP_1 {6} \\
   CONFIG.USER_HBM_FBDIV_1 {36} \\
   CONFIG.USER_HBM_HEX_CP_RES_1 {0x0000A600} \\
   CONFIG.USER_HBM_HEX_FBDIV_CLKOUTDIV_1 {0x00000902} \\
   CONFIG.USER_HBM_HEX_LOCK_FB_REF_DLY_1 {0x00001f1f} \\
   CONFIG.USER_HBM_LOCK_FB_DLY_1 {31} \\
   CONFIG.USER_HBM_LOCK_REF_DLY_1 {31} \\
   CONFIG.USER_HBM_RES_1 {10} \\
   CONFIG.USER_HBM_STACK {2} \\
   CONFIG.USER_MC_ENABLE_00 {TRUE} \\
   CONFIG.USER_MC_ENABLE_01 {TRUE} \\
   CONFIG.USER_MC_ENABLE_02 {TRUE} \\
   CONFIG.USER_MC_ENABLE_03 {TRUE} \\
   CONFIG.USER_MC_ENABLE_04 {TRUE} \\
   CONFIG.USER_MC_ENABLE_05 {TRUE} \\
   CONFIG.USER_MC_ENABLE_06 {TRUE} \\
   CONFIG.USER_MC_ENABLE_07 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_08 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_09 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_10 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_11 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_12 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_13 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_14 {TRUE} \\
   CONFIG.USER_PHY_ENABLE_15 {TRUE} \\
   CONFIG.USER_SWITCH_ENABLE_01 {TRUE} \\
] [get_ips ${desiredName}]
set_property CONFIG.USER_APB_EN {false} [get_ips ${desiredName}]
set_property CONFIG.USER_HBM_DENSITY {${if (is16GB) "16GB" else "8GB"}} [get_ips ${desiredName}]
*/

class XilinxHBM
(
  bundleParams: Seq[AXI4BundleParameters],
  portNum: Int = 1,
  is16GB: Boolean = false,
  override val desiredName: String,
  // no AXI lite, always AXI4 Full
) extends BlackBox {
  val io = IO(new XilinxHBMIO(bundleParams, is16GB))
  require(!(portNum > 16 && !is16GB))
  require(portNum > 0 && portNum <= 1) // currently only single port is supported

  ElaborationArtefacts.add(s"$desiredName.vivado.tcl",
    s"""
       |create_ip -name hbm -vendor xilinx.com -library ip -version 1.0 -module_name $desiredName
       |set_property -dict [list \\
       |CONFIG.USER_APB_EN {false} \\
       |    CONFIG.USER_CLK_SEL_LIST0 {AXI_00_ACLK} \\
       |    CONFIG.USER_CLK_SEL_LIST1 {AXI_16_ACLK} \\
       |    CONFIG.USER_HBM_CP_1 {6} \\
       |    CONFIG.USER_HBM_DENSITY {16GB} \\
       |    CONFIG.USER_HBM_FBDIV_1 {36} \\
       |    CONFIG.USER_HBM_HEX_CP_RES_1 {0x0000A600} \\
       |    CONFIG.USER_HBM_HEX_FBDIV_CLKOUTDIV_1 {0x00000902} \\
       |    CONFIG.USER_HBM_HEX_LOCK_FB_REF_DLY_1 {0x00001f1f} \\
       |    CONFIG.USER_HBM_LOCK_FB_DLY_1 {31} \\
       |    CONFIG.USER_HBM_LOCK_REF_DLY_1 {31} \\
       |    CONFIG.USER_HBM_RES_1 {10} \\
       |    CONFIG.USER_HBM_STACK {2} \\
       |    CONFIG.USER_MC0_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC0_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC0_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC0_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC0_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC0_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC0_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC0_MANUAL_ADDR_MAP_SEL {false} \\
       |    CONFIG.USER_MC0_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC0_REORDER_EN {false} \\
       |    CONFIG.USER_MC0_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC10_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC10_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC10_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC10_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC10_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC10_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC10_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC10_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC10_REORDER_EN {false} \\
       |    CONFIG.USER_MC10_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC11_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC11_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC11_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC11_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC11_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC11_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC11_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC11_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC11_REORDER_EN {false} \\
       |    CONFIG.USER_MC11_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC12_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC12_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC12_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC12_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC12_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC12_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC12_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC12_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC12_REORDER_EN {false} \\
       |    CONFIG.USER_MC12_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC13_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC13_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC13_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC13_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC13_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC13_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC13_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC13_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC13_REORDER_EN {false} \\
       |    CONFIG.USER_MC13_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC14_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC14_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC14_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC14_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC14_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC14_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC14_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC14_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC14_REORDER_EN {false} \\
       |    CONFIG.USER_MC14_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC15_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC15_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC15_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC15_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC15_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC15_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC15_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC15_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC15_REORDER_EN {false} \\
       |    CONFIG.USER_MC15_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC1_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC1_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC1_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC1_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC1_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC1_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC1_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC1_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC1_REORDER_EN {false} \\
       |    CONFIG.USER_MC1_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC2_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC2_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC2_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC2_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC2_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC2_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC2_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC2_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC2_REORDER_EN {false} \\
       |    CONFIG.USER_MC2_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC3_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC3_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC3_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC3_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC3_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC3_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC3_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC3_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC3_REORDER_EN {false} \\
       |    CONFIG.USER_MC3_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC4_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC4_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC4_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC4_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC4_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC4_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC4_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC4_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC4_REORDER_EN {false} \\
       |    CONFIG.USER_MC4_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC5_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC5_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC5_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC5_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC5_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC5_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC5_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC5_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC5_REORDER_EN {false} \\
       |    CONFIG.USER_MC5_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC6_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC6_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC6_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC6_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC6_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC6_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC6_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC6_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC6_REORDER_EN {false} \\
       |    CONFIG.USER_MC6_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC7_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC7_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC7_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC7_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC7_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC7_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC7_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC7_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC7_REORDER_EN {false} \\
       |    CONFIG.USER_MC7_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC8_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC8_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC8_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC8_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC8_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC8_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC8_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC8_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC8_REORDER_EN {false} \\
       |    CONFIG.USER_MC8_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC9_BG_INTERLEAVE_EN {true} \\
       |    CONFIG.USER_MC9_CA0_CA5_MAP {0x1c61440d9} \\
       |    CONFIG.USER_MC9_LADDR_BA0_BA4_MAP {0x00282248} \\
       |    CONFIG.USER_MC9_LADDR_CA0_CA4_MAP {0xc61440d9} \\
       |    CONFIG.USER_MC9_LOOKAHEAD_ACT {true} \\
       |    CONFIG.USER_MC9_LOOKAHEAD_PCH {true} \\
       |    CONFIG.USER_MC9_MAINTAIN_COHERENCY {true} \\
       |    CONFIG.USER_MC9_Q_AGE_LIMIT {0x7F} \\
       |    CONFIG.USER_MC9_REORDER_EN {false} \\
       |    CONFIG.USER_MC9_REORDER_QUEUE_EN {false} \\
       |    CONFIG.USER_MC_ENABLE_08 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_09 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_10 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_11 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_12 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_13 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_14 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_15 {TRUE} \\
       |    CONFIG.USER_MC_ENABLE_APB_01 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_08 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_09 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_10 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_11 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_12 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_13 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_14 {TRUE} \\
       |    CONFIG.USER_PHY_ENABLE_15 {TRUE} \\
       |    CONFIG.USER_SAXI_01 {false} \\
       |    CONFIG.USER_SAXI_02 {false} \\
       |    CONFIG.USER_SAXI_03 {false} \\
       |    CONFIG.USER_SAXI_04 {false} \\
       |    CONFIG.USER_SAXI_05 {false} \\
       |    CONFIG.USER_SAXI_06 {false} \\
       |    CONFIG.USER_SAXI_07 {false} \\
       |    CONFIG.USER_SAXI_08 {false} \\
       |    CONFIG.USER_SAXI_09 {false} \\
       |    CONFIG.USER_SAXI_10 {false} \\
       |    CONFIG.USER_SAXI_11 {false} \\
       |    CONFIG.USER_SAXI_12 {false} \\
       |    CONFIG.USER_SAXI_13 {false} \\
       |    CONFIG.USER_SAXI_14 {false} \\
       |    CONFIG.USER_SAXI_15 {false} \\
       |    CONFIG.USER_SAXI_16 {false} \\
       |    CONFIG.USER_SAXI_17 {false} \\
       |    CONFIG.USER_SAXI_18 {false} \\
       |    CONFIG.USER_SAXI_19 {false} \\
       |    CONFIG.USER_SAXI_20 {false} \\
       |    CONFIG.USER_SAXI_21 {false} \\
       |    CONFIG.USER_SAXI_22 {false} \\
       |    CONFIG.USER_SAXI_23 {false} \\
       |    CONFIG.USER_SAXI_24 {false} \\
       |    CONFIG.USER_SAXI_25 {false} \\
       |    CONFIG.USER_SAXI_26 {false} \\
       |    CONFIG.USER_SAXI_27 {false} \\
       |    CONFIG.USER_SAXI_28 {false} \\
       |    CONFIG.USER_SAXI_29 {false} \\
       |    CONFIG.USER_SAXI_30 {false} \\
       |    CONFIG.USER_SAXI_31 {false} \\
       |    CONFIG.USER_SWITCH_ENABLE_01 {TRUE} \\
       |    CONFIG.USER_XSDB_INTF_EN {FALSE} ] \\
       |[get_ips ${desiredName}]
       |""".stripMargin)
}

class LazyXilinxHBMController(moduleNamePrefix: String, portNum: Int = 1, is16GB: Boolean = false)(implicit p: Parameters) extends LazyModule {
  val node = Seq.fill(portNum)(AXI4SlaveNode(
    Seq(AXI4SlavePortParameters(
      Seq(AXI4SlaveParameters(
        address = Seq(AddressSet(0x00000000L, 0x3FFFFFFFFL)),
        resources = new SimpleDevice("hbm", Seq()).reg("hbm"),
        regionType = RegionType.UNCACHED,
        executable = true,
        supportsRead = TransferSizes(32, 512),
        supportsWrite = TransferSizes(32, 512),

      )),
      beatBytes = 32,
      requestKeys = Seq(),
      responseFields = Nil,
    ))
  ))

  override def shouldBeInlined: Boolean = true

  val slaveClockNodes = Seq.fill(portNum)(ClockSinkNode(Seq(ClockSinkParameters())))
  val HBMRefClockNode = ClockSinkNode(Seq(ClockSinkParameters()))


  lazy val module = new LazyRawModuleImp(this) {
    val bundleParams = node.map { a => a.in.head._2.bundle }
    val hbm = Module(new XilinxHBM(bundleParams, portNum, is16GB, moduleNamePrefix + "xilinx_hbm"))
    val slaveClocks = slaveClockNodes.map(a => a.in.head._1.clock)
    val slaveResets = slaveClockNodes.map(a => a.in.head._1.reset)

    val hbmIO = hbm.io


    hbmIO.HBM_REF_CLK_0 := HBMRefClockNode.in.head._1.clock
    if (is16GB) {
      hbmIO.HBM_REF_CLK_1.get := HBMRefClockNode.in.head._1.clock
    }

    require(portNum <= 1)
    for (i <- 0 until portNum) {
      val (in, _) = node(i).in.head
      val hbmPort = hbmIO.AXI_00

      hbmIO.AXI_00_ACLK := slaveClocks(i)
      hbmIO.AXI_00_ARESET_N := !(slaveResets(i).asBool)

      hbmIO.APB_0_PCLK := slaveClocks(i)
      hbmIO.APB_1_PCLK := slaveClocks(i)

      hbmIO.APB_0_PRESET_N := !(slaveResets(i).asBool)
      hbmIO.APB_1_PRESET_N := !(slaveResets(i).asBool)


      hbmPort.drivenByStandardAXI4(in, slaveClocks(i), slaveResets(i).asBool)
    }

  }
}