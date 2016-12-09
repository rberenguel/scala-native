package scala.scalanative
package optimizer
package pass

import util.{unreachable, ScopedVar}, ScopedVar.scoped
import linker.World
import nir._, Inst.Let

/** Eliminates returns of Unit values and replaces them with void. */
class UnitLowering(implicit fresh: Fresh) extends Pass {
  import UnitLowering._

  private var defnRetty: Type = _

  override def onNode(node: World.Node): Unit = node match {
    case node: World.Method =>
      val Type.Function(_, retty) = node.ty
      defnRetty = retty

    case _ =>
      ()
  }

  override def preInst = {
    case inst @ Let(n, op) if op.resty == Type.Unit =>
      Seq(
        Let(op),
        Let(n, Op.Copy(Val.Unit))
      )

    case Inst.Ret(_) if defnRetty == Type.Unit =>
      Seq(Inst.Ret(Val.None))
  }

  override def preVal = {
    case Val.Unit => unit
  }

  override def preType = {
    case Type.Unit =>
      Type.Ptr

    case Type.Function(params, Type.Unit) =>
      Type.Function(params, Type.Void)
  }
}

object UnitLowering extends PassCompanion {
  val unitName = Global.Top("scala.scalanative.runtime.BoxedUnit$")
  val unit     = Val.Global(unitName tag "value", Type.Ptr)

  override val depend =
    Seq(unitName)

  override def apply(config: tools.Config, top: World.Top) =
    new UnitLowering()(top.fresh)
}
