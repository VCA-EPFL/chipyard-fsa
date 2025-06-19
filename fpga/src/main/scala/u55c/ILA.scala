package chipyard.fpga.u55c

import chisel3._
import chisel3.util._

import scala.collection.immutable.SeqMap
import freechips.rocketchip.util.ElaborationArtefacts
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.prci.{ClockNode, ClockSinkNode}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._

class ILABalckBoxIO(probes: Seq[Int]) extends Record {
  override val elements: SeqMap[String,Data] = SeqMap.from(
    probes.zipWithIndex.map {
      case (width, idx) => s"probe$idx" -> Input(UInt(width.W))
    } :+ ("clk" -> Input(Clock()))
  )
}

class ILA(override val desiredName: String, probes: Seq[Int], depth: Int = 1024, mu: Int = 1) extends BlackBox {

  val io = IO(new ILABalckBoxIO(probes))

  val ipCreateCmd = s"create_ip -name ila -vendor xilinx.com -library ip -version 6.2 -module_name ${desiredName}"

  val probeConfig = (
    Seq(
      "set_property -dict [list ",
      s"CONFIG.ALL_PROBE_SAME_MU_CNT {$mu}",
      s"CONFIG.C_DATA_DEPTH {$depth}",
      s"CONFIG.C_NUM_OF_PROBES {${probes.size}}"
    ) ++
    probes.zipWithIndex.map { case (width, idx) =>
      s"CONFIG.C_PROBE${idx}_WIDTH {$width}"
    } :+
    s"] [get_ips ${desiredName}]"
  ).mkString(" ")

  ElaborationArtefacts.add(s"${desiredName}.vivado.tcl",
    Seq(ipCreateCmd, probeConfig).mkString("\n")
  )
}

object ILA {
  def apply[T <: Data](name: String, clk: Clock, probes: Seq[T], depth: Int = 1024, mu: Int = 1): ILA = {
    val probeWidths = probes.map(_.getWidth)
    val ila = Module(new ILA(name, probeWidths, depth, mu))
    ila.io.elements.foreach{ case (name, data) =>
      if (name.startsWith("probe")) {
        val idx = name.stripPrefix("probe").toInt
        data := probes(idx).asUInt
      } else {
        data := clk
      }
    }
    ila
  }
}

class AXI4ILA(
  desiredNamePrefix: String,
  signalFn: AXI4Bundle => Seq[Data],
)(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()
  lazy val module = new LazyModuleImp(this) {
    node.in.zipWithIndex.foreach{ case ((axi, edge), i) =>
      ILA(
        s"${desiredNamePrefix}_$i",
        clk = this.clock,
        probes = signalFn(axi)
      )
    }
  }
}

object AXI4ILA {
  def apply(
             desiredNamePrefix: String,
             // clockNode: ClockSinkNode,
             signalFn: AXI4Bundle => Seq[Data] = {
      axi => {
        Seq(
          axi.ar.fire, axi.aw.fire, axi.r.fire, axi.w.fire, axi.b.fire,
          axi.ar.valid, axi.ar.ready, axi.aw.valid, axi.aw.ready,
          axi.ar.bits.addr, axi.aw.bits.addr, axi.r.valid, axi.r.ready, axi.r.bits.data, axi.w.ready, axi.w.valid,
          axi.w.bits.data, axi.w.bits.last, axi.w.bits.strb, axi.aw.bits.size, axi.aw.bits.len

        )
      }
    }
  )(implicit p: Parameters): AXI4IdentityNode = {
    val ila = LazyModule(new AXI4ILA(desiredNamePrefix, signalFn))
    /*
    InModuleBody {
      ila.module.clock := clockNode.in.head._1.clock
    }*/
    ila.node
  }
}