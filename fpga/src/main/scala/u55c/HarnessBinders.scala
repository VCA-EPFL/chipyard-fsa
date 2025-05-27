package chipyard.fpga.u55c

import chisel3._


import chipyard.harness.{HarnessBinder}
import chipyard.iobinders._


class WithU55CAXIMemHarnessBinder extends HarnessBinder({
  case (th: U55CTestHarnessImpl, port: AXI4MemPort, chipId: Int) => {
    // TODO
    port.io <> DontCare
  }
})