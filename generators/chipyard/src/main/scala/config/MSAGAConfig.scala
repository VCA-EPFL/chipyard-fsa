package chipyard

import chipyard.config.WithUniformBusFrequencies
import chipyard.harness._
import chipyard.iobinders._
import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._
import testchipip.tsi._
import testchipip.serdes._
import testchipip.util.ClockedIO
import chisel3._
import chisel3.reflect.DataMirror
import freechips.rocketchip.amba.axi4.AXI4Bundle
import freechips.rocketchip.devices.tilelink.{BuiltInErrorDeviceParams, DevNullParams}
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.prci.{ClockSinkNode, ClockSinkParameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink.BuiltInZeroDeviceParams
import freechips.rocketchip.subsystem.HasTileLinkLocations
import freechips.rocketchip.subsystem.MBUS
import msaga.{CanHaveMSAGADirectAXI4, MSAGAKey, WithFpMSAGA, WithFpMSAGAMBusInjector, AXI4DirectMemPortKey}
import msaga.tsi.MSAGASimTSI

class WithMSAGASimTSIOverSerialTL extends HarnessBinder({
  case (th: HasHarnessInstantiators, port: SerialTLPort, chipId: Int) if (port.portId == 0) => {
    port.io match {
      case io: HasClockOut =>
      case io: HasClockIn => io.clock_in := th.harnessBinderClock
      case io: CreditedSourceSyncPhitIO => io.clock_in := th.harnessBinderClock; io.reset_in := th.harnessBinderReset
    }

    port.io match {
      case io: DecoupledPhitIO =>
        // If the port is locally synchronous (provides a clock), drive everything with that clock
        // Else, drive everything with the harnes clock
        val clock = port.io match {
          case io: HasClockOut => io.clock_out
          case io: HasClockIn => th.harnessBinderClock
        }
        withClock(clock) {
          val ram = Module(LazyModule(new SerialRAM(port.serdesser, port.params)(port.serdesser.p)).module)
          ram.io.ser.in <> io.out
          io.in <> ram.io.ser.out
          val success = MSAGASimTSI.connect(ram.io.tsi, clock, th.harnessBinderReset, chipId)
          when(success) { th.success := true.B }
        }
    }
  }
})


class WithMBusErrorDevice extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey).copy(
    errorDevice = Some(BuiltInErrorDeviceParams(
      errorParams = DevNullParams(
        address = Seq(AddressSet(0x4000, 0xfff)),
        maxAtomic = 8,
        maxTransfer = 4096
      )
    ))
  )
})

/**
  * A simple MSAGA config which integrates the MSAGA into the mbus.
  */
class MSAGAConfig extends Config(
  new WithMSAGASimTSIOverSerialTL ++
  new WithFpMSAGAMBusInjector ++
  new WithFpMSAGA ++
  new WithNMemoryChannels(2) ++
  new WithMBusErrorDevice ++
  new WithUniformBusFrequencies(500) ++
  new NoCoresConfig
)


/**
  * Create axi4 IO ports and directly connect them to the MSAGA memory ports.
  * (without going through the TL mbus)
  *
  * In simulation, we still want the axi ports to be connected to the dramsim
  * by the harness binder, so we resue the `AXI4MemPort` as the port type.
  */
class WithMSAGADirectAXI4IOBinder extends OverrideLazyIOBinder({
  (system: CanHaveMSAGADirectAXI4) => {
    implicit val p: Parameters = GetSystemParameters(system)
    val clockSinkNode = ClockSinkNode(Seq(ClockSinkParameters()))
    /* currently we use the same clock as mbus */
    clockSinkNode := system.asInstanceOf[HasTileLinkLocations].locateTLBusWrapper(MBUS).fixedClockNode
    def clockBundle = clockSinkNode.in.head._1
    InModuleBody {
      val ports: Seq[AXI4MemPort] = Option.when(system.msaga_axi4.isDefined) {
        system.msaga_axi4.get.zipWithIndex.map({ case (m, i) =>
          val port = IO(new ClockedIO(DataMirror.internal.chiselTypeClone[AXI4Bundle](m))).suggestName(s"axi4_msaga_${i}")
          port.bits <> m
          port.clock := clockBundle.clock
          AXI4MemPort(
            () => port,
            p(AXI4DirectMemPortKey).get,
            system.msaga.get.memNode.edges.out(i),
            p(MemoryBusKey).dtsFrequency.get.toInt
          )
        }).toSeq
      }.getOrElse(Nil)
      (ports, Nil)
    }
  }
})

class WithMSAGADirectAXI4(
  memBase: BigInt = 0x80000000L,
  memSize: BigInt = 0x10000000L,
  memBeatBytes: Int = 8,
  memMaxTransferBytes: Int = 256,
  memIdBits: Int = 1
) extends Config((site, here, up) => {
  case AXI4DirectMemPortKey => Some(MemoryPortParams(
    MasterPortParams(memBase, memSize, beatBytes = 8, idBits = 4),
    nMemoryChannels = site(MSAGAKey).get.nMemPorts
  ))
  case BuildSystem => (p: Parameters) => new DigitalTop()(p) with CanHaveMSAGADirectAXI4
})

class WithMBusZeroDevice extends Config((site, here, up) => {
  case MemoryBusKey => up(MemoryBusKey).copy(
    zeroDevice = Some(BuiltInZeroDeviceParams(
      addr = AddressSet(0x5000, 0xfff)
    ))
  )
})


/**
  * A MSAGA config which directly connects the MSAGA to the AXI4 ports.
  * Note: the directly connected AXI4 ports are not reachable by the mbus.
  */
class MSAGADirectAXI4Config extends Config(
  new WithMSAGASimTSIOverSerialTL ++
  new WithMSAGADirectAXI4IOBinder ++
  new WithMSAGADirectAXI4 ++
  new WithFpMSAGA ++
  // mbus requires at least 1 slave device
  new WithMBusZeroDevice ++
  new WithNoMemPort ++
  new NoCoresConfig
)

class WithFbusBeatBytes(n: Int) extends Config((site, here, up) => {
  case FrontBusKey => up(FrontBusKey).copy(
    beatBytes = n
  )
})

class MSAGAFPGAConfig(freqMHz: Int) extends Config(
  new WithCustomSlavePort(data_width = 32, id_bits = 1) ++
  new WithFbusBeatBytes(4) ++
  new WithNoSerialTL ++
  new WithMBusErrorDevice ++
  new WithHarnessBinderClockFreqMHz(freqMHz) ++
  new WithUniformBusFrequencies(freqMHz) ++
  new MSAGADirectAXI4Config
)

class MSAGAFPGA50MHzConfig extends MSAGAFPGAConfig(50)