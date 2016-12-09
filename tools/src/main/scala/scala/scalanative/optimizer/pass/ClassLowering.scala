package scala.scalanative
package optimizer
package pass

import linker.World._
import util.{sh, unsupported}
import nir._, Inst.Let, Shows._

/** Lowers field acceses to pointer arithmetics on the layout struct. */
class FieldLowering(implicit top: Top) extends Pass {
  override def preInst = {
    case Let(n, Op.Field(obj, FieldRef(cls: Class, fld))) =>
      val classty = cls.layoutStruct

      Seq(
        Let(n, Op.Elem(classty, obj, Seq(Val.I32(0), Val.I32(fld.index + 1))))
      )
  }

  override def preType = {
    case ty: Type.RefKind if ty != Type.Unit => Type.Ptr
  }
}

object FieldLowering extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    new FieldLowering()(top)
}
