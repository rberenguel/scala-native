package scala.scalanative
package optimizer
package pass

import scala.collection.mutable
import scala.util.control.Breaks._
import linker.World._
import util.unsupported
import nir._, Inst.Let

/** Hoists all stack allocations to the entry basic block and
 *  maps class allocations to calls to the gc allocator.
 */
class AllocLowering(implicit fresh: Fresh, top: Top) extends Pass {
  import AllocLowering._

  private val stackallocs = mutable.UnrolledBuffer.empty[Inst]

  override def preInsts = {
    case insts =>
      stackallocs.clear
      insts
  }

  override def preInst = {
    case inst @ Let(_, alloc: Op.Stackalloc) =>
      stackallocs += inst
      Seq()

    case Let(n, Op.Classalloc(ClassRef(cls))) =>
      val size = Val.Local(fresh(), Type.I64)

      Seq(
        Let(size.name, Op.Sizeof(cls.layoutStruct)),
        Let(n, Op.Call(allocSig, alloc, Seq(cls.typeConst, size)))
      )
  }

  override def postInsts = {
    case insts =>
      val label +: rest = insts
      val newinsts      = label +: (stackallocs ++: rest)

      newinsts
  }
}

object AllocLowering extends PassCompanion {
  val allocName = Global.Top("scalanative_alloc")
  val allocSig  = Type.Function(Seq(Arg(Type.Ptr), Arg(Type.I64)), Type.Ptr)
  val alloc     = Val.Global(allocName, allocSig)

  override def inject(top: Top) =
    top.enter(Defn.Declare(Attrs.None, allocName, allocSig))

  override def apply(config: tools.Config, top: Top) =
    new AllocLowering()(top.fresh, top)
}
