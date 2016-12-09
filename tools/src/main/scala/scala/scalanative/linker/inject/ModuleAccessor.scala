package scala.scalanative
package linker
package inject

import nir._

/** Injects module accessor and backing field. */
object ModuleAccessor extends Inject {
  override def inject(top: World.Top): Unit =
    top.classes.foreach { cls =>
      if (cls.isModule) {
        import top.fresh

        val clsTy   = Type.Class(cls.name)
        val clsNull = Val.Zero(Type.Class(cls.name))

        val valueName = cls.name tag "value"
        val valueDefn = Defn.Var(Attrs.None, valueName, clsTy, clsNull)
        val value     = Val.Global(valueName, Type.Ptr)

        val entry      = fresh()
        val existing   = fresh()
        val initialize = fresh()

        val self  = Val.Local(fresh(), clsTy)
        val cond  = Val.Local(fresh(), Type.Bool)
        val alloc = Val.Local(fresh(), clsTy)

        val hasInit = top.nodes.contains(cls.name member "init")
        val initCall = if (!hasInit) {
          Inst.None
        } else {
          val initSig =
            Type.Function(Seq(Arg(Type.Class(cls.name))), Type.Void)
          val init = Val.Global(cls.name member "init", Type.Ptr)

          Inst.Let(Op.Call(initSig, init, Seq(alloc)))
        }

        val loadName = cls.name tag "load"
        val loadSig  = Type.Function(Seq(), clsTy)
        val loadDefn =
          Defn.Define(
            Attrs.None,
            loadName,
            loadSig,
            Seq(
              Inst.Label(entry, Seq()),
              Inst.Let(self.name, Op.Load(clsTy, value)),
              Inst.Let(cond.name, Op.Comp(Comp.Ine, Rt.Object, self, clsNull)),
              Inst.If(cond, Next(existing), Next(initialize)),
              Inst.Label(existing, Seq()),
              Inst.Ret(self),
              Inst.Label(initialize, Seq()),
              Inst.Let(alloc.name, Op.Classalloc(cls.name)),
              Inst.Let(Op.Store(clsTy, value, alloc)),
              initCall,
              Inst.Ret(alloc)))

        top.enter(valueDefn)
        top.enter(loadDefn)
      }
    }
}
