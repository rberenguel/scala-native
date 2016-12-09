package scala.scalanative
package optimizer
package pass

import scala.collection.mutable
import nir._, Shows._
import util.sh
import linker.World._

/** Eliminates pure computations that are not being used. */
class DeadCodeElimination(implicit top: Top) extends Pass {
  override def preInsts = {
    case insts =>
      val cfg      = analysis.ControlFlow.Graph(insts)
      val usedef   = analysis.UseDef(cfg)
      val newinsts = mutable.UnrolledBuffer.empty[Inst]

      cfg.all.foreach { block =>
        if (usedef(block.name).alive) {
          newinsts += block.label
          block.insts.foreach {
            case inst @ Inst.Let(n, op) =>
              if (usedef(n).alive) newinsts += inst
            case inst: Inst.Cf =>
              newinsts += inst
            case _ =>
              ()
          }
        }
      }

      newinsts
  }
}

object DeadCodeElimination extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    new DeadCodeElimination()(top)
}
