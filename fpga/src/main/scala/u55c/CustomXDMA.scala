package chipyard.fpga.u55c

import sifive.fpgashells.ip.xilinx.xdma.HasXDMAPads
import sifive.fpgashells.ip.xilinx.xdma.HasXDMAClocks
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.ElaborationArtefacts
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.{AddressRange, IdRange}
import sifive.fpgashells.devices.xilinx.xdma.XDMAPads
import sifive.fpgashells.ip.xilinx.xdma.XDMAClocks

/*
   Copyright 2016 SiFive, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/


/* 
Sifive XDMA works in AXIBridge mode (`Functional Mode = AXIBridge` in Xilinx XDMA IP),
this is a custom XDMA implementation that works in DMA mode (`Functional Mode = DMA` in Xilinx XDMA IP)
and supports the official xdma linux kernel driver.

Since the two modes have different AXI interfaces, IO ports, we do not adapt the existing
Sifive XDMA implementation, but create a new one although some part of the code can be reused.
*/

case class CustomXDMAParams(
  name:     String,
  location: String,
  lanes:    Int    = 1,
  gen:      Int    = 4,
  axi4IDBits:  Int    = 4,
  nIRQs:   Int    = 1,
  // (base, size)
  axiLiteBar: AddressRange = AddressRange(0x8000, 4 * 1024)
)
{
  val addrBits: Int = 64
  require (gen >= 1  && gen <= 4)
  require (axi4IDBits >= 1 && axi4IDBits <= 4)
  require(nIRQs >= 1 && nIRQs <= 16)

  private val bandwidth = lanes * 250 << (gen-1) // MB/s
  private val busBytesAt250MHz = bandwidth / 250
  val busBytes = busBytesAt250MHz max 8
  private val minMHz = 250.0 * busBytesAt250MHz / busBytes
  val axiMHz = minMHz max 62.5
}

trait HasXDMAUsrIrq { this : Bundle =>
  def nIRQs: Int
  val usr_irq_req: UInt = Input(UInt(nIRQs.W))
  val usr_irq_ack: UInt = Output(UInt(nIRQs.W))
}

trait HasUsrLinkUp { this : Bundle =>
  val user_lnk_up: Bool = Output(Bool())
}

trait HasCustomXDMABus {
  def masterBus: AXI4BundleParameters
  def masterLiteBus: AXI4BundleParameters
  val m_axi = new XilinxAXI4Bundle(masterBus, isAXI4Lite = false, qos = false)
  val m_axil = new XilinxAXI4Bundle(masterLiteBus, isAXI4Lite = true)
}

class CustomXDMABlackBoxIO(
    val lanes: Int,
    val nIRQs: Int,
    val masterBus: AXI4BundleParameters,
    val masterLiteBus: AXI4BundleParameters
) extends Bundle
with HasXDMAPads
with HasXDMAClocks
with HasXDMAUsrIrq
with HasUsrLinkUp
with HasCustomXDMABus


class CustomXDMABlackBox
(
  c: CustomXDMAParams,
  masterBundleParams: AXI4BundleParameters,
  masterLiteBundleParams: AXI4BundleParameters
) extends BlackBox
{
  override def desiredName: String = c.name

  val io = IO(new CustomXDMABlackBoxIO(c.lanes, c.nIRQs, masterBundleParams, masterLiteBundleParams))

  val pcieGTs = c.gen match {
    case 1 => "2.5_GT/s"
    case 2 => "5.0_GT/s"
    case 3 => "8.0_GT/s"
    case 4 => "16.0_GT/s"
    case _ => "wrong"
  }

  // 62.5, 125, 250 (no trailing zeros)
  val formatter = new java.text.DecimalFormat("0.###")
  val axiMHzStr = formatter.format(c.axiMHz)

  def nextPowerOf2(x: BigInt): BigInt = {
    if (x <= 0) BigInt(1)
    else BigInt(1) << x.bitLength
  }
  // 4K - 2G
  val K = BigInt(1024)
  val M = K * K
  val G = M * K
  require(c.axiLiteBar.size <= 2 * G, s"AXI-Lite BAR size must be less than or equal to 2GB, got ${c.axiLiteBar.size} ${2 * G}")
  val sizeRounded = nextPowerOf2(c.axiLiteBar.size) min (4 * K)
  val (size, scale) = sizeRounded match {
    case n if n >= G => (n / G, "Gigabytes")
    case n if n >= M => (n / M, "Megabytes")
    case n => (n / K, "Kilobytes")
  }
  val baseHex = c.axiLiteBar.base.toString(16)

  ElaborationArtefacts.add(s"${desiredName}.vivado.tcl",
    s"""create_ip -vendor xilinx.com -library ip -version 4.1 -name xdma -module_name ${desiredName} -dir $$ipdir -force
       |set_property -dict [list                                           \\
       |  CONFIG.functional_mode                    {DMA}                  \\
       |  CONFIG.cfg_mgmt_if                        {false}                \\
       |  CONFIG.pcie_extended_tag                  {false}                \\
       |  CONFIG.pf0_msi_enabled                    {true}                 \\
       |  CONFIG.pf0_msix_enabled                   {true}                 \\
       |  CONFIG.xdma_num_usr_irq                   {${c.nIRQs}}           \\
       |  CONFIG.pcie_blk_locn                      {${c.location}}        \\
       |  CONFIG.ref_clk_freq                       {100_MHz}              \\
       |  CONFIG.axisten_freq                       {${axiMHzStr}}         \\
       |  CONFIG.axi_addr_width                     {${c.addrBits}}        \\
       |  CONFIG.axi_data_width                     {${c.busBytes*8}_bit}  \\
       |  CONFIG.axi_id_width                       {${c.axi4IDBits}}      \\
       |  CONFIG.axilite_master_en                  {true}                 \\
       |  CONFIG.axilite_master_scale               {$scale}               \\
       |  CONFIG.axilite_master_size                {$size}                \\
       |  CONFIG.pciebar2axibar_axil_master         {${baseHex}            \\
       |  CONFIG.pl_link_cap_max_link_width         {X${c.lanes}}          \\
       |  CONFIG.pl_link_cap_max_link_speed         {${pcieGTs}}           \\
       |] [get_ips ${desiredName}]
       |""".stripMargin)
}

class CustomXDMA(val c: CustomXDMAParams)(implicit p: Parameters) extends LazyModule {

  val master = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = c.name,
      id = IdRange(0, 1 << c.axi4IDBits)
    ))
  )))

  val masterLite = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = c.name,
      id = IdRange(0, 1)
    ))
  )))

  override lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val io = IO(new Bundle {
      val pads = new XDMAPads(c.lanes)
      val clocks = new XDMAClocks
    })

    val masterBundle = master.out.head._2.bundle
    val masterLiteBundle = masterLite.out.head._2.bundle

    require(masterBundle.dataBits == c.busBytes * 8)
    require(masterLiteBundle.dataBits == 32)
    require(masterLiteBundle.addrBits <= 32)

    val blackbox = Module(new CustomXDMABlackBox(c, masterBundle, masterLiteBundle))

    val (m, _) = master.out.head
    val (l, _) = masterLite.out.head

    // Pads
    io.pads.pci_exp_txp := blackbox.io.pci_exp_txp
    io.pads.pci_exp_txn := blackbox.io.pci_exp_txn
    blackbox.io.pci_exp_rxp := io.pads.pci_exp_rxp
    blackbox.io.pci_exp_rxn := io.pads.pci_exp_rxn

    // Clocks
    blackbox.io.sys_clk    := io.clocks.sys_clk
    blackbox.io.sys_clk_gt := io.clocks.sys_clk_gt
    blackbox.io.sys_rst_n  := io.clocks.sys_rst_n
    io.clocks.axi_aclk     := blackbox.io.axi_aclk
    io.clocks.axi_aresetn  := blackbox.io.axi_aresetn

    // Interrupts
    // TODO: make it a IntSinkNode if needed
    blackbox.io.usr_irq_req := 0.U

    blackbox.io.m_axi.driveStandardAXI4(m, blackbox.io.axi_aclk, blackbox.io.axi_aresetn)
    blackbox.io.m_axil.driveStandardAXI4(l, blackbox.io.axi_aclk, blackbox.io.axi_aresetn)
  }
}