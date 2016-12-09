package scala.scalanative
package optimizer
package pass

import linker.World._
import nir._

/** Maps type values to corresponding RTTI constants. */
class TypeLowering(implicit top: Top, fresh: Fresh) extends Pass {
  override def preVal = {
    case Val.Global(ScopeRef(node), _) =>
      node.typeConst
  }
}

object TypeLowering extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    new TypeLowering()(top, top.fresh)
}
