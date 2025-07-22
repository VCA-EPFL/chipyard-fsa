package chipyard.fpga.u55c

import fsa.{Configs, WithFpFSA}
import org.chipsalliance.cde.config._

class WithU55CTweaks extends Config(
  new WithU55CAXIMemHarnessBinder ++
  new chipyard.harness.WithTieOffL2FBusAXI ++
  // clocking
  new chipyard.harness.WithAllClocksFromHarnessClockInstantiator ++
  new chipyard.harness.WithHarnessBinderClockFreqMHz(70) ++
  new chipyard.config.WithUniformBusFrequencies(70) ++
  new testchipip.serdes.WithNoSerialTL ++
  new testchipip.soc.WithNoScratchpads
)

// useful for xdma test
class EmptyU55CConfig extends Config (
  new WithU55CTweaks ++
  new chipyard.EmptyChipTopConfig ++
  new WithFpFSA(params = Configs.fsa16x16.copy(nMemPorts = 1))
)
