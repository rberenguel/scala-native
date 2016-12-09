package scala.scalanative
package linker
package inject

import nir._

object MainMethod extends Inject with Depend {
  val ObjectArray =
    Type.Class(Global.Top("scala.scalanative.runtime.ObjectArray"))

  val Rt =
    Type.Module(Global.Top("scala.scalanative.runtime.package$"))
  val RtInitSig =
    Type.Function(Seq(Arg(Rt), Arg(Type.I32), Arg(Type.Ptr)), ObjectArray)
  val RtInit =
    Val.Global(Rt.name member "init_i32_ptr_class.ssnr.ObjectArray", Type.Ptr)

  val MainName = Global.Top("main")
  val MainSig  = Type.Function(Seq(Arg(Type.I32), Arg(Type.Ptr)), Type.I32)

  val InitSig  = Type.Function(Seq(), Type.Unit)
  val Init     = Val.Global(Global.Top("scalanative_init"), Type.Ptr)
  val InitDecl = Defn.Declare(Attrs.None, Init.name, InitSig)

  override def depend =
    Seq(ObjectArray.name, Rt.name, RtInit.name)

  override def inject(top: World.Top): Unit =
    top.main.foreach { entry =>
      import top.fresh

      val entryMainTy = Type
        .Function(Seq(Arg(Type.Module(entry)), Arg(ObjectArray)), Type.Void)
      val entryMainName =
        Global.Member(entry, "main_class.ssnr.ObjectArray_unit")
      val entryMain = Val.Global(entryMainName, Type.Ptr)

      val argc   = Val.Local(fresh(), Type.I32)
      val argv   = Val.Local(fresh(), Type.Ptr)
      val module = Val.Local(fresh(), Type.Module(entry.top))
      val rt     = Val.Local(fresh(), Rt)
      val arr    = Val.Local(fresh(), ObjectArray)

      top.enter(
        Defn.Define(
          Attrs.None,
          MainName,
          MainSig,
          Seq(Inst.Label(fresh(), Seq(argc, argv)),
              Inst.Let(Op.Call(InitSig, Init, Seq())),
              Inst.Let(rt.name, Op.Module(Rt.name)),
              Inst.Let(arr.name,
                       Op.Call(RtInitSig, RtInit, Seq(rt, argc, argv))),
              Inst.Let(module.name, Op.Module(entry.top)),
              Inst.Let(Op.Call(entryMainTy, entryMain, Seq(module, arr))),
              Inst.Ret(Val.I32(0)))))
    }
}
