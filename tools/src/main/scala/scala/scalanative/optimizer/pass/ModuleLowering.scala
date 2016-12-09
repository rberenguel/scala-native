package scala.scalanative
package optimizer
package pass

import linker.World._
import nir._

/** Lowers module loads to calls to module accessors. */
class ModuleLowering extends Pass {
  override def preInst = {
    case Inst.Let(n, Op.Module(name)) =>
      val loadSig = Type.Function(Seq(), Type.Class(name))
      val load    = Val.Global(name tag "load", Type.Ptr)

      Seq(
        Inst.Let(n, Op.Call(loadSig, load, Seq()))
      )
  }

  override def preType = {
    case Type.Module(n) => Type.Class(n)
  }
}

object ModuleLowering extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    new ModuleLowering()
}
