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
import freechips.rocketchip.util.BooleanToAugmentedBoolean
import msaga.{CanHaveMSAGADirectAXI4, MSAGAKey, WithFpMSAGA, WithFpMSAGAMBusInjector, AXI4DirectMemPortKey}
import msaga.tsi.MSAGASimTSI
import msaga.Configs
import msaga.utils.AXI4WriteTracker
import msaga.MSAGAParams
import msaga.arithmetic.FPArithmeticImpl


// a custom TSI to drive the simulation
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

// ComposeHarnessBinder seems to be removed... do it manually
class ComposeHarnessBinder[T <: HasHarnessInstantiators, S <: Port[_]]
(fn: => HarnessBinderFunction) extends Config((site, here, up) => {
  case HarnessBinders =>
    val prevFn = up(HarnessBinders)
    val combined: HarnessBinderFunction = new PartialFunction[(HasHarnessInstantiators, Port[_], Int), Unit] {
      override def isDefinedAt(x: (HasHarnessInstantiators, Port[_], Int)): Boolean = prevFn.isDefinedAt(x) || fn.isDefinedAt(x)
      override def apply(x: (HasHarnessInstantiators, Port[_], Int)): Unit = {
        if (prevFn.isDefinedAt(x)) {
          prevFn(x)
        }
        if (fn.isDefinedAt(x)) {
          fn(x)
        }
      }
    }
    combined
})

/*
   Exporting dram data using TSI is too slow, insert this DPI-C block
   to get the data directly from AXI bus
 */
class WithAXI4WriteTracker extends ComposeHarnessBinder({
  case (th: HasHarnessInstantiators, port: AXI4MemPort, _: Int) =>
    val axiTracker = Module(new AXI4WriteTracker(port.edge.bundle))
    axiTracker.io.clock := port.io.clock
    axiTracker.io.aw_fire := port.io.bits.aw.fire
    axiTracker.io.aw_addr := port.io.bits.aw.bits.addr
    axiTracker.io.aw_size := port.io.bits.aw.bits.size
    axiTracker.io.aw_len := port.io.bits.aw.bits.len
    axiTracker.io.w_fire := port.io.bits.w.fire
    axiTracker.io.w_data := port.io.bits.w.bits.data
    axiTracker.io.w_last := port.io.bits.w.bits.last
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

class WithFbusBeatBytes(n: Int) extends Config((site, here, up) => {
  case FrontBusKey => up(FrontBusKey).copy(
    beatBytes = n
  )
})

class BaseMSAGAConfig(msagaParams: MSAGAParams, arithmetic: FPArithmeticImpl) extends Config(
  new WithMSAGASimTSIOverSerialTL ++
  new WithFpMSAGA(msagaParams, arithmetic) ++
  /* MSAGA requires > 0.6*row-bytes bandwidth to avoid being memory bandwidth bottlenecked,
     here we give 1 row-bytes by default.
  */
  new WithNBitMemoryBus(
    (msagaParams.saRows * arithmetic.accType.getWidth / msagaParams.nMemPorts) max 64
  ) ++
  // use 4 bytes by default
  new WithFbusBeatBytes(4) ++
  /*
    In simulation, we use slow TSI to push instructions to the MSAGA,
    to speed up simulation and make sure the MSAGA is not bottlenecked by the TSI,
    we use a high clock frequency for the harness binder and the front bus.
    TODO: write a SimXDMA to drive the AXI4 front port directly
  */
  new chipyard.clocking.WithClockGroupsCombinedByName(("uncore",
    Seq("sbus", "mbus", "pbus", "cbus", "obus", "implicit", "clock_tap"),
    Seq("fbus"))) ++
  new WithHarnessBinderClockFreqMHz(1000) ++
  new WithFrontBusFrequency(1000) ++
  new WithUniformBusFrequencies(100) ++
  new WithNMemoryChannels(msagaParams.nMemPorts) ++
  new NoCoresConfig
)

class MSAGAConfig(
  msagaParams: MSAGAParams,
  arithmetic: FPArithmeticImpl = Configs.fp16MulFp32AddArithmeticImpl
) extends Config(
  // collect simulation results
  new WithAXI4WriteTracker ++
  // inject the MSAGA into the mbus
  new WithFpMSAGAMBusInjector ++
  // mbus AXI4 -> TL
  new WithMBusErrorDevice ++
  new BaseMSAGAConfig(msagaParams, arithmetic)
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
  params: MasterPortParams,
  useMBusBeatBytes: Boolean
) extends Config((site, here, up) => {
  case AXI4DirectMemPortKey => {
    val msagaParams = site(MSAGAKey).get
    val memParams = useMBusBeatBytes.option(params.copy(
      beatBytes = site(MemoryBusKey).beatBytes
    )).getOrElse(params)
    Some(MemoryPortParams(
      master = memParams,
      nMemoryChannels = msagaParams.nMemPorts
    ))
  }
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
class MSAGADirectAXI4Config(
  msagaParams: MSAGAParams = Configs.msaga4x4,
  arithmetic: FPArithmeticImpl = Configs.fp16MulFp32AddArithmeticImpl,
  memParams: MasterPortParams = MasterPortParams(
    base = 0x80000000L,
    size = 0x10000000L,
    beatBytes = 8,
    idBits = 1,
    maxXferBytes = 512
  ),
  useMBusBeatBytes: Boolean = true
) extends Config(
  // collect simulation results
  new WithAXI4WriteTracker ++
  new WithMSAGADirectAXI4IOBinder ++
  new WithMSAGADirectAXI4(memParams, useMBusBeatBytes) ++
  // mbus requires at least 1 slave device
  new WithMBusZeroDevice ++
  new WithNoMemPort ++
  new BaseMSAGAConfig(msagaParams, arithmetic)
)


class MSAGA4x4Fp16Config extends MSAGAConfig(Configs.msaga4x4)
class MSAGA8x8Fp16Config extends MSAGAConfig(Configs.msaga8x8)
class MSAGA16x16Fp16Config extends MSAGAConfig(Configs.msaga16x16)
class MSAGA32x32Fp16Config extends MSAGAConfig(Configs.msaga32x32)
class MSAGA64x64Fp16Config extends MSAGAConfig(Configs.msaga64x64)
class MSAGA128x128Fp16Config extends MSAGAConfig(Configs.msaga128x128)

class AXI4MSAGA4x4Fp16Config extends MSAGADirectAXI4Config(Configs.msaga4x4)
class AXI4MSAGA8x8Fp16Config extends MSAGADirectAXI4Config(Configs.msaga8x8)
class AXI4MSAGA16x16Fp16Config extends MSAGADirectAXI4Config(Configs.msaga16x16)
class AXI4MSAGA32x32Fp16Config extends MSAGADirectAXI4Config(Configs.msaga32x32)
class AXI4MSAGA64x64Fp16Config extends MSAGADirectAXI4Config(Configs.msaga64x64)
class AXI4MSAGA128x128Fp16Config extends MSAGADirectAXI4Config(Configs.msaga128x128)

class MSAGA4x4Bf16Config extends MSAGAConfig(Configs.msaga4x4, Configs.bf16MulFp32AddArithmeticImpl)
class MSAGA8x8Bf16Config extends MSAGAConfig(Configs.msaga8x8, Configs.bf16MulFp32AddArithmeticImpl)
class MSAGA16x16Bf16Config extends MSAGAConfig(Configs.msaga16x16, Configs.bf16MulFp32AddArithmeticImpl)
class MSAGA32x32Bf16Config extends MSAGAConfig(Configs.msaga32x32, Configs.bf16MulFp32AddArithmeticImpl)
class MSAGA64x64Bf16Config extends MSAGAConfig(Configs.msaga64x64, Configs.bf16MulFp32AddArithmeticImpl)
class MSAGA128x128Bf16Config extends MSAGAConfig(Configs.msaga128x128, Configs.bf16MulFp32AddArithmeticImpl)

class AXI4MSAGA4x4Bf16Config extends MSAGADirectAXI4Config(Configs.msaga4x4, Configs.bf16MulFp32AddArithmeticImpl)
class AXI4MSAGA8x8Bf16Config extends MSAGADirectAXI4Config(Configs.msaga8x8, Configs.bf16MulFp32AddArithmeticImpl)
class AXI4MSAGA16x16Bf16Config extends MSAGADirectAXI4Config(Configs.msaga16x16, Configs.bf16MulFp32AddArithmeticImpl)
class AXI4MSAGA32x32Bf16Config extends MSAGADirectAXI4Config(Configs.msaga32x32, Configs.bf16MulFp32AddArithmeticImpl)
class AXI4MSAGA64x64Bf16Config extends MSAGADirectAXI4Config(Configs.msaga64x64, Configs.bf16MulFp32AddArithmeticImpl)
class AXI4MSAGA128x128Bf16Config extends MSAGADirectAXI4Config(Configs.msaga128x128, Configs.bf16MulFp32AddArithmeticImpl)