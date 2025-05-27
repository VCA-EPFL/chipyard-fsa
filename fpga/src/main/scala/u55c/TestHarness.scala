package chipyard.fpga.u55c

import chisel3._
import chipyard.harness.HasHarnessInstantiators
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.prci._
import sifive.fpgashells.ip.xilinx._
import sifive.fpgashells.clocks.PLLFactoryKey
import sifive.fpgashells.shell._
import sifive.fpgashells.shell.xilinx._

class SysClockU55CPlacedOverlay
(
  val shell: U55CShellBasicOverlays,
  name: String,
  val designInput: ClockInputDesignInput,
  val shellInput: ClockInputShellInput
) extends LVDSClockInputXilinxPlacedOverlay(name, designInput, shellInput) {
  val node = shell { ClockSourceNode(freqMHz = 100, jitterPS = 50)(ValName(name)) }

  shell { InModuleBody {
    shell.xdc.addPackagePin(io.n, "F23")
    shell.xdc.addPackagePin(io.p, "F24")
    shell.xdc.addIOStandard(io.n, "LVDS")
    shell.xdc.addIOStandard(io.p, "LVDS")
  }}
}

class SysClockU55CShellPlacer
(
  shell: U55CShellBasicOverlays,
  val shellInput: ClockInputShellInput
)(implicit val valName: ValName) extends ClockInputShellPlacer[U55CShellBasicOverlays] {
  override def place(di: ClockInputDesignInput) = new SysClockU55CPlacedOverlay(shell, valName.value, di, shellInput)
}

abstract class U55CShellBasicOverlays()(implicit p: Parameters) extends UltraScaleShell {
  // PLL reset causes
  val pllReset = InModuleBody { Wire(Bool()) }
  // PCIe reset
  val pcie_rst_n = InModuleBody { Wire(Bool()) }

  val sys_clock = Overlay(ClockInputOverlayKey, new SysClockU55CShellPlacer(this, ClockInputShellInput()))
  val xdma = Overlay(CustomXDMAOverlayKey, new U55CPCIeCustomXDMAShellPlacer(this, PCIeShellInput()))
}

class U55CFPGATestHarness(implicit p: Parameters) extends U55CShellBasicOverlays {
  def dp = designParameters
  // place all clocks in the shell
  require(dp(ClockInputOverlayKey).nonEmpty)
  val sysClkNode = dp(ClockInputOverlayKey).head.place(ClockInputDesignInput()).overlayOutput.node

  /*** Connect/Generate clocks ***/
  // connect to the PLL that will generate multiple clocks
  val harnessSysPLL = dp(PLLFactoryKey)()
  harnessSysPLL := sysClkNode

  val wrangler = LazyModule(new ResetWrangler)

  val dutFreqMHz = 50
  val dutFixedClockNode = FixedClockBroadcast(Some(ClockParameters(freqMHz = dutFreqMHz)))
  val dutClockGroup = ClockGroup()
  val dutClockNode = ClockSinkNode(freqMHz = dutFreqMHz)
  dutFixedClockNode := wrangler.node := dutClockGroup := harnessSysPLL

  dutClockNode := dutFixedClockNode

  val placedXDMA = dp(CustomXDMAOverlayKey).head.place(CustomXDMADesignInput(
    wrangler.node, dutFixedClockNode
  ))

  val dutDomain = LazyModule(new ClockSinkDomain)
  dutDomain.clockNode := dutFixedClockNode

  dutDomain {
    val ram = AXI4RAM(
      address = AddressSet(0x80000000L, 0xfffff),
      beatBytes = placedXDMA.overlayOutput.config.busBytes
    )
    ram := AXI4Fragmenter() := AXI4Buffer() := AXI4ILA("xdma_master") := placedXDMA.overlayOutput.master
    val inst = AXI4RAM(
      address = AddressSet(0x8000, 0xfff),
      beatBytes = 4
    )
    inst := AXI4Fragmenter() := AXI4Buffer() := AXI4ILA("xdma_master_lite") := placedXDMA.overlayOutput.masterLite
  }
  override lazy val module: LazyRawModuleImp = new U55CTestHarnessImpl(this)
}



class U55CTestHarnessImpl(outer: U55CFPGATestHarness) extends LazyRawModuleImp(outer) with HasHarnessInstantiators {

  val prst_n = IO(Input(Bool())).suggestName("prst_n")
  outer.xdc.addPackagePin(prst_n, "BF41")
  outer.xdc.addIOStandard(prst_n, "LVCMOS18")
  outer.xdc.addPullup(prst_n)
  outer.sdc.addAsyncPath(Seq(prst_n))

  val pcie_rst_n_ibuff = Module(new IBUF)
  pcie_rst_n_ibuff.suggestName("pcie_rst_n_ibuff")
  pcie_rst_n_ibuff.io.I := prst_n
  outer.pcie_rst_n := pcie_rst_n_ibuff.io.O

  val sysclk: Clock = outer.sysClkNode.out.head._1.clock

  val powerOnReset: Bool = PowerOnResetFPGAOnly(sysclk)
  outer.sdc.addAsyncPath(Seq(powerOnReset))

  outer.pllReset := powerOnReset

  // reset setup
  val hReset = Wire(Reset())
  hReset := outer.dutClockNode.in.head._1.reset

  def referenceClockFreqMHz = outer.dutFreqMHz
  def referenceClock = outer.dutClockNode.in.head._1.clock
  def referenceReset = hReset
  def success = { require(false, "Unused"); false.B }

  childClock := referenceClock
  childReset := referenceReset

  instantiateChipTops()
}