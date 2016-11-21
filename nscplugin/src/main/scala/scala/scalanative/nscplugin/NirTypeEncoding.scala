package scala.scalanative
package nscplugin

import util._

trait NirTypeEncoding { self: NirCodeGen =>
  import global._
  import definitions._
  import nirAddons._
  import nirDefinitions._

  final case class SimpleType(sym: Symbol, targs: Seq[SimpleType] = Seq())

  object SimpleType {
    lazy val Unit    = SimpleType(UnitClass)
    lazy val Boolean = SimpleType(BooleanClass)
    lazy val Char    = SimpleType(CharClass)
    lazy val Byte    = SimpleType(ByteClass)
    lazy val UByte   = SimpleType(UByteClass)
    lazy val Short   = SimpleType(ShortClass)
    lazy val UShort  = SimpleType(UShortClass)
    lazy val Int     = SimpleType(IntClass)
    lazy val UInt    = SimpleType(UIntClass)
    lazy val Long    = SimpleType(LongClass)
    lazy val ULong   = SimpleType(ULongClass)
    lazy val Float   = SimpleType(FloatClass)
    lazy val Double  = SimpleType(DoubleClass)
    lazy val Object  = SimpleType(ObjectClass)

    import scala.language.implicitConversions

    implicit def fromType(t: Type): SimpleType = t.normalize match {
      case ThisType(ArrayClass)  => SimpleType.Object
      case ThisType(sym)         => SimpleType(sym)
      case SingleType(_, sym)    => SimpleType(sym)
      case ConstantType(_)       => fromType(t.underlying)
      case TypeRef(_, sym, args) => SimpleType(sym, args.map(fromType))
      case ClassInfoType(_, _, ArrayClass) =>
        abort("ClassInfoType to ArrayClass!")
      case ClassInfoType(_, _, sym) => SimpleType(sym)
      case t: AnnotatedType         => fromType(t.underlying)
      case tpe: ErasedValueType     => SimpleType(tpe.valueClazz)
    }

    implicit def fromSymbol(sym: Symbol): SimpleType = SimpleType(sym)
  }

  def genTypeName(sym: Symbol): nir.Global

  def genArrayCode(st: SimpleType): Char = {
    val SimpleType(ArrayClass, Seq(targ)) = st

    genPrimCode(targ)
  }

  def genType(st: SimpleType, boxUnsigned: Boolean = true): nir.Type =
    st.sym match {
      case ArrayClass                  => genType(RuntimeArrayClass(genPrimCode(st.targs.head)))
      case UnitClass | BoxedUnitClass  => nir.Type.Unit
      case NothingClass                => nir.Type.Nothing
      case NullClass                   => genType(RuntimeNullClass)
      case ObjectClass                 => nir.Rt.Object
      case CharClass                   => nir.Type.I16
      case BooleanClass                => nir.Type.Bool
      case ByteClass                   => nir.Type.I8
      case UByteClass if !boxUnsigned  => nir.Type.I8
      case ShortClass                  => nir.Type.I16
      case UShortClass if !boxUnsigned => nir.Type.I16
      case IntClass                    => nir.Type.I32
      case UIntClass if !boxUnsigned   => nir.Type.I32
      case LongClass                   => nir.Type.I64
      case ULongClass if !boxUnsigned  => nir.Type.I64
      case FloatClass                  => nir.Type.F32
      case DoubleClass                 => nir.Type.F64
      case PtrClass                    => nir.Type.Ptr
      case sym if TupleClass.seq.contains(sym) && st.targs.nonEmpty =>
        nir.Type.Struct(nir.Global.None, st.targs.map(genType(_, boxUnsigned)))
      case sym if isModule(sym) =>
        nir.Type.Module(genTypeName(sym))
      case sym if sym.isInterface =>
        nir.Type.Trait(genTypeName(sym))
      case sym =>
        nir.Type.Class(genTypeName(sym))
    }

  def genTypeValue(st: SimpleType): nir.Val =
    genPrimCode(st) match {
      case 'O' if st.sym != UnitClass =>
        nir.Val.Global(genTypeName(st.sym), nir.Type.Ptr)
      case 'O' =>
        genTypeValue(RuntimePrimitive('U'))
      case code =>
        genTypeValue(RuntimePrimitive(code))
    }

  def genPrimCode(st: SimpleType): Char = st.sym match {
    case CharClass    => 'C'
    case BooleanClass => 'B'
    case UByteClass   => 'z'
    case ByteClass    => 'Z'
    case UShortClass  => 's'
    case ShortClass   => 'S'
    case UIntClass    => 'i'
    case IntClass     => 'I'
    case ULongClass   => 'l'
    case LongClass    => 'L'
    case FloatClass   => 'F'
    case DoubleClass  => 'D'
    case _            => 'O'
  }

  def genBoxType(tpe: Type): nir.Type = {
    val (sym, _) = decomposeType(tpe)

    sym match {
      case BooleanClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Boolean"))
      case CharClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Character"))
      case UByteClass =>
        nir.Type.Class(nir.Global.Top("scala.scalanative.native.UByte"))
      case ByteClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Byte"))
      case UShortClass =>
        nir.Type.Class(nir.Global.Top("scala.scalanative.native.UShort"))
      case ShortClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Short"))
      case UIntClass =>
        nir.Type.Class(nir.Global.Top("scala.scalanative.native.UInt"))
      case IntClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Integer"))
      case ULongClass =>
        nir.Type.Class(nir.Global.Top("scala.scalanative.native.ULong"))
      case LongClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Long"))
      case FloatClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Float"))
      case DoubleClass =>
        nir.Type.Class(nir.Global.Top("java.lang.Double"))
      case _ =>
        unsupported("Box type must be primitive type.")
    }
  }

  def isModule(sym: Symbol): Boolean =
    sym.isModule || sym.isModuleClass || sym.isImplClass

  def isExternModule(sym: Symbol): Boolean =
    isModule(sym) && sym.annotations.exists(_.symbol == ExternClass)

  def isField(sym: Symbol): Boolean =
    !sym.isMethod && sym.isTerm && !isModule(sym)
}
