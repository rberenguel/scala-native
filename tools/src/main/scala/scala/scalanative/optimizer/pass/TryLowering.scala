package scala.scalanative
package optimizer
package pass

import scala.collection.mutable
import linker.World._
import analysis.ControlFlow
import util.unreachable
import nir._

/** Lowers try-catch with nested function calls into LLVM-style invokes. */
class TryLowering(implicit fresh: Fresh) extends Pass {
  override def preInsts = {
    case insts =>
      val cfg      = ControlFlow.Graph(insts)
      val eh       = cfg.eh
      val newinsts = mutable.UnrolledBuffer.empty[Inst]

      cfg.foreach { block =>
        val handler = eh(block.name)

        newinsts += block.label

        block.insts.foreach {
          case Inst.Let(n, call: Op.Call) if handler.nonEmpty =>
            val fail = Next.Fail(handler.get)
            val name = fresh()
            val succ = Next.Succ(name)
            val params =
              if (call.resty == Type.Void) Seq()
              else Seq(Val.Local(n, call.resty))

            newinsts += Inst.Invoke(call.ty, call.ptr, call.args, succ, fail)
            newinsts += Inst.Label(succ.name, params)

          case Inst.Try(Next.Succ(n), fail) =>
            newinsts += Inst.Jump(Next(n))

          case inst =>
            newinsts += inst
        }
      }

      newinsts
  }
}

object TryLowering extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    new TryLowering()(top.fresh)
}
