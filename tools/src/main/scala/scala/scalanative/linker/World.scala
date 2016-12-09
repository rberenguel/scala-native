package scala.scalanative
package linker

import scala.collection.mutable
import util.{sh, unreachable}
import nir._, Shows._

object World {
  sealed abstract class Ref[T] {
    def unapply(ty: nir.Type)(implicit top: Top): Option[T] = ty match {
      case ty: Type.Named => unapply(ty.name)
      case _              => None
    }
    def unapply(name: Global)(implicit top: Top): Option[T]
  }

  sealed abstract class Node {
    def attrs: Attrs
    def name: Global
    var id: Int   = -1
    var in: Scope = _

    def isTop: Boolean    = this.isInstanceOf[Top]
    def isClass: Boolean  = this.isInstanceOf[Class]
    def isTrait: Boolean  = this.isInstanceOf[Trait]
    def isMethod: Boolean = this.isInstanceOf[Method]
    def isField: Boolean  = this.isInstanceOf[Field]

    def typeName: Global = this match {
      case node: World.Class =>
        node.name tag "class" tag "type"
      case node: World.Trait =>
        node.name tag "trait" tag "type"
      case node: World.Struct =>
        node.name tag "struct" tag "type"
      case _ =>
        util.unreachable
    }

    def typeConst: Val =
      Val.Global(typeName, Type.Ptr)
  }

  object NodeRef extends Ref[Node] {
    def unapply(name: Global)(implicit top: Top): Option[Node] =
      top.nodes.get(name)
  }

  sealed abstract class Scope extends Node {
    var members: Seq[Node] = Seq()

    def methods: Seq[Method] =
      members.collect { case meth: Method => meth }

    def fields: Seq[Field] =
      members.collect { case fld: Field => fld }
  }

  object ScopeRef extends Ref[Scope] {
    def unapply(name: Global)(implicit top: Top): Option[Scope] =
      top.nodes.get(name).collect {
        case node: Scope => node
      }
  }

  final class Struct(val attrs: Attrs,
                     val name: Global,
                     val tys: Seq[nir.Type])
      extends Scope

  object StructRef extends Ref[Struct] {
    def unapply(name: Global)(implicit top: Top): Option[Struct] =
      top.nodes.get(name).collect {
        case node: Struct => node
      }
  }

  final class Trait(val attrs: Attrs,
                    val name: Global,
                    val traitNames: Seq[Global])
      extends Scope {
    var traits: Seq[Trait] = Seq()

    def alltraits: Seq[Trait] =
      traits.flatMap(_.alltraits).distinct :+ this

    def allmethods: Seq[Method] = {
      val parent =
        alltraits.init.map(_.allmethods).foldLeft(Seq.empty[Method])(_ ++ _)
      parent ++ methods
    }
  }

  object TraitRef extends Ref[Trait] {
    def unapply(name: Global)(implicit top: Top): Option[Trait] =
      top.nodes.get(name).collect {
        case node: Trait => node
      }
  }

  final class Class(val attrs: Attrs,
                    val name: Global,
                    val parentName: Option[Global],
                    val traitNames: Seq[Global],
                    val isModule: Boolean)
      extends Scope {
    var range: Range           = _
    var parent: Option[Class]  = None
    var subclasses: Seq[Class] = Seq()
    var traits: Seq[Trait]     = Seq()

    def ty = Type.Class(name)

    def alltraits: Seq[Trait] = {
      val base = parent.fold(Seq.empty[Trait])(_.alltraits)
      (base ++ traits.flatMap(_.alltraits)).distinct
    }

    def allfields: Seq[Field] =
      parent.fold(Seq.empty[Field])(_.allfields) ++ fields

    def allmethods: Seq[Method] =
      parent.fold(Seq.empty[Method])(_.allmethods) ++ methods

    def allvslots: Seq[Method] =
      parent.fold(Seq.empty[Method])(_.allvslots) ++ vslots

    def vslots: Seq[Method] = members.collect {
      case meth: Method if meth.isVirtual =>
        meth
    }

    def vtableStruct: Type.Struct =
      Type.Struct(Global.None, vtable.map(_.ty))

    def vtableValue: Val.Struct = Val.Struct(Global.None, vtable)

    def typeStruct: Type.Struct =
      Type.Struct(Global.None, Seq(Type.I32, Type.Ptr, vtableStruct))

    def typeValue: Val.Struct =
      Val.Struct(Global.None,
                 Seq(Val.I32(id), Val.String(name.id), vtableValue))

    def layoutStruct: Type.Struct =
      Type.Struct(name tag "layout", Type.Ptr +: allfields.map(_.ty))

    def vtable: Seq[Val] = {
      val base = parent.fold(Seq.empty[Val])(_.vtable)

      val overrides = methods.flatMap { meth =>
        meth.classOverrides.map((meth, _))
      }
      val baseWithOverrides = overrides.foldLeft(base) {
        case (base, (meth, ovmeth)) =>
          base.updated(ovmeth.vindex, meth.value)
      }

      baseWithOverrides ++ vslots.map(_.value)
    }

    def imap: Map[Method, Val] = {
      val traitOverrides = allmethods.flatMap { meth =>
        meth.traitOverrides.map((meth, _))
      }
      val traitMethods = alltraits.flatMap(_.allmethods).distinct

      traitMethods.map { tmethod =>
        var impl: Val = Val.Null
        traitOverrides.foreach {
          case (meth, ovmeth) =>
            if (ovmeth == tmethod) {
              impl = meth.value
            }
        }
        tmethod -> impl
      }.toMap
    }

    def alloverrides: Seq[(Method, Method)] = {
      val base = parent.fold(Seq.empty[(Method, Method)])(_.alloverrides)
      base ++ methods.flatMap {
        case meth if meth.overrides.nonEmpty =>
          meth.overrides.map(ov => (meth, ov))
        case _ =>
          Seq()
      }
    }
  }

  object ClassRef extends Ref[Class] {
    def unapply(name: Global)(implicit top: Top): Option[Class] =
      top.nodes.get(name).collect {
        case node: Class => node
      }
  }

  final class Method(val attrs: Attrs,
                     val name: Global,
                     var ty: nir.Type,
                     var insts: Seq[nir.Inst],
                     val isConcrete: Boolean)
      extends Node {
    var overrides: Seq[Method] = Seq()
    var overriden: Seq[Method] = Seq()

    def isVirtual = !isConcrete || overriden.nonEmpty

    def isStatic = !isVirtual

    def value =
      if (isConcrete) Val.Global(name, Type.Ptr)
      else Val.Zero(Type.Ptr)

    def classOverrides: Seq[Method] = overrides.collect {
      case meth if meth.in.isClass =>
        meth
    }

    def traitOverrides: Seq[Method] = overrides.collect {
      case meth if meth.in.isTrait =>
        meth
    }

    def vslot: Method = {
      assert(isVirtual)

      classOverrides.headOption.fold(this)(_.vslot)
    }

    def vindex: Int = {
      assert(isVirtual)
      assert(in.isClass)

      val cls = in.asInstanceOf[Class]
      val res = cls.allvslots.indexOf(this)
      assert(res >= 0,
             s"failed to find vslot for ${this.name} in ${in.name} (" +
               s"all vslots: ${cls.allvslots.map(_.name)}, " +
               s"all methods: ${cls.allmethods.map(_.name)})")
      res
    }
  }

  object MethodRef extends Ref[(Scope, Method)] {
    def unapply(name: Global)(implicit top: Top): Option[(Scope, Method)] =
      top.nodes.get(name).collect {
        case node: Method => (node.in, node)
      }
  }

  final class Field(val attrs: Attrs,
                    val name: Global,
                    var ty: nir.Type,
                    var rhs: Val,
                    val isConst: Boolean)
      extends Node {
    def index = {
      assert(in.isClass)
      in.asInstanceOf[Class].allfields.indexOf(this)
    }
  }

  object FieldRef extends Ref[(Scope, Field)] {
    def unapply(name: Global)(implicit top: Top): Option[(Scope, Field)] =
      top.nodes.get(name).collect {
        case node: Field => (node.in, node)
      }
  }

  final class Top(val main: Option[Global]) extends Scope {
    val nodes            = mutable.Map.empty[Global, Node]
    val structs          = mutable.UnrolledBuffer.empty[Struct]
    val classes          = mutable.UnrolledBuffer.empty[Class]
    val traits           = mutable.UnrolledBuffer.empty[Trait]
    override val methods = mutable.UnrolledBuffer.empty[Method]
    override val fields  = mutable.UnrolledBuffer.empty[Field]
    val unresolved       = mutable.Set.empty[Global]
    val links            = mutable.Set.empty[Attr.Link]

    def enter(node: Node): Unit = {
      def strip(n: Global): Global = {
        val id = n.id
        assert(id.startsWith("extern."))
        Global.Top(id.substring(7)) // strip extern. prefix
      }

      val name =
        if (node.attrs.isExtern && (node.isMethod || node.isField))
          strip(node.name)
        else node.name

      if (nodes.contains(name)) {
        println(s"overwriting $name : ${nodes(name)} -> $node")
      }

      nodes += name -> node
      node match {
        case node: Class  => classes += node // id given in assignClassIds
        case node: Trait  => node.id = traits.length; traits += node
        case node: Method => methods += node // id given in assignMethodIds
        case node: Field  => node.id = fields.length; fields += node
        case node: Struct => node.id = structs.length; structs += node
        case _            => unreachable
      }
    }

    def enter(defn: Defn): Unit = defn match {
      case defn: Defn.Trait =>
        enter(new Trait(defn.attrs, defn.name, defn.traits))

      case defn: Defn.Class =>
        enter(
          new Class(defn.attrs,
                    defn.name,
                    defn.parent,
                    defn.traits,
                    isModule = false))

      case defn: Defn.Module =>
        enter(
          new Class(defn.attrs,
                    defn.name,
                    defn.parent,
                    defn.traits,
                    isModule = true))

      case defn: Defn.Var =>
        enter(
          new Field(defn.attrs,
                    defn.name,
                    defn.ty,
                    defn.value,
                    isConst = false))

      case defn: Defn.Const =>
        enter(
          new Field(defn.attrs,
                    defn.name,
                    defn.ty,
                    defn.value,
                    isConst = true))

      case defn: Defn.Declare =>
        enter(
          new Method(defn.attrs,
                     defn.name,
                     defn.ty,
                     Seq.empty,
                     isConcrete = false))

      case defn: Defn.Define =>
        enter(
          new Method(defn.attrs,
                     defn.name,
                     defn.ty,
                     defn.insts,
                     isConcrete = true))

      case defn: Defn.Struct =>
        enter(new Struct(defn.attrs, defn.name, defn.tys))

      case _ =>
        ()
    }

    def finish(): Unit = {
      def enrichMethods(): Unit = methods.foreach { node =>
        if (node.name.isTop) {
          node.in = this
        } else {
          val owner = nodes(node.name.top).asInstanceOf[Scope]
          node.in = owner
          owner.members = owner.members :+ node
          node.attrs.overrides.foreach { n =>
            val ovnode = nodes(n).asInstanceOf[Method]
            node.overrides = node.overrides :+ ovnode
            ovnode.overriden = ovnode.overriden :+ node
          }
        }
      }

      def enrichFields(): Unit = fields.foreach { node =>
        println(s"enriching ${node.name}")
        if (node.name.isTop) {
          node.in = this
        } else {
          val parent = nodes(node.name.top).asInstanceOf[Class]
          node.in = parent
          parent.members = parent.members :+ node
        }
      }

      def enrichClasses(): Unit = classes.foreach { node =>
        val parent = node.parentName.map(nodes(_).asInstanceOf[Class])
        val traits = node.traitNames.map(nodes(_).asInstanceOf[Trait])
        node.in = this
        node.parent = parent
        node.traits = traits
        parent.foreach { parent =>
          parent.subclasses = parent.subclasses :+ node
        }
      }

      def enrichTraits(): Unit = traits.foreach { node =>
        node.in = this
        node.traits = node.traitNames.map(n => nodes(n).asInstanceOf[Trait])
      }

      def assignClassIds(): Unit = {
        var id = 0

        def loop(node: Class): Unit = {
          val start = id
          id += 1
          node.subclasses.foreach(loop)
          val end = id - 1
          node.id = start
          node.range = start to end
        }

        loop(nodes(Rt.Object.name).asInstanceOf[Class])
      }

      def assignMethodIds(): Unit = {
        val traitMethods = methods.filter(_.in.isTrait)
        val restMethods  = methods.filterNot(_.in.isTrait)

        var id = 0
        traitMethods.foreach { meth =>
          meth.id = id
          id += 1
        }
        restMethods.foreach { meth =>
          meth.id = id
          id += 1
        }
      }

      enrichMethods()
      enrichFields()
      enrichClasses()
      enrichTraits()
      assignClassIds()
      assignMethodIds()
    }

    implicit val fresh = nir.Fresh("tx")
    def name           = Global.None
    def attrs          = Attrs.None

    lazy val dispatchName = Global.Top("__dispatch")
    lazy val dispatchVal  = Val.Global(dispatchName, Type.Ptr)
    lazy val (dispatchTy, dispatchDefn) = {
      val traitMethods = methods.filter(_.in.isTrait).sortBy(_.id)

      val columns = classes.sortBy(_.id).map { cls =>
        val row = Array.fill[Val](traitMethods.length)(Val.Null)
        cls.imap.foreach {
          case (meth, value) =>
            row(meth.id) = value
        }
        Val.Array(Type.Ptr, row)
      }
      val table = Val.Array(Type.Array(Type.Ptr, traitMethods.length), columns)

      (table.ty, Defn.Const(Attrs.None, dispatchName, table.ty, table))
    }

    lazy val instanceName = Global.Top("__instance")
    lazy val instanceVal  = Val.Global(instanceName, Type.Ptr)
    lazy val (instanceTy, instanceDefn) = {
      val columns = classes.sortBy(_.id).map { cls =>
        val row = new Array[Boolean](traits.length)
        cls.alltraits.foreach { trt =>
          row(trt.id) = true
        }
        Val.Array(Type.Bool, row.map(Val.Bool))
      }
      val table = Val.Array(Type.Array(Type.Bool, traits.length), columns)

      (table.ty, Defn.Const(Attrs.None, instanceName, table.ty, table))
    }
  }
}
