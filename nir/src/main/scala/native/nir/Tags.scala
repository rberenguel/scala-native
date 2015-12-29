package native
package nir

object Tags {
  final val UsgnAttr = 1

  final val AddBin  = 1 + UsgnAttr
  final val SubBin  = 1 + AddBin
  final val MulBin  = 1 + SubBin
  final val DivBin  = 1 + MulBin
  final val ModBin  = 1 + DivBin
  final val ShlBin  = 1 + ModBin
  final val LshrBin = 1 + ShlBin
  final val AshrBin = 1 + LshrBin
  final val AndBin  = 1 + AshrBin
  final val OrBin   = 1 + AndBin
  final val XorBin  = 1 + OrBin

  final val EqComp  = 1 + XorBin
  final val NeqComp = 1 + EqComp
  final val LtComp  = 1 + NeqComp
  final val LteComp = 1 + LtComp
  final val GtComp  = 1 + LteComp
  final val GteComp = 1 + GtComp

  final val TruncConv    = 1 + GteComp
  final val ZextConv     = 1 + TruncConv
  final val SextConv     = 1 + ZextConv
  final val FptruncConv  = 1 + SextConv
  final val FpextConv    = 1 + FptruncConv
  final val FptouiConv   = 1 + FpextConv
  final val FptosiConv   = 1 + FptouiConv
  final val UitofpConv   = 1 + FptosiConv
  final val SitofpConv   = 1 + UitofpConv
  final val PtrtointConv = 1 + SitofpConv
  final val InttoptrConv = 1 + PtrtointConv
  final val BitcastConv  = 1 + InttoptrConv

  final val VarDefn      = 1 + BitcastConv
  final val DeclareDefn  = 1 + VarDefn
  final val DefineDefn   = 1 + DeclareDefn
  final val StructDefn   = 1 + DefineDefn
  final val IntefaceDefn = 1 + StructDefn
  final val ClassDefn    = 1 + IntefaceDefn
  final val ModuleDefn   = 1 + ClassDefn

  final val NoneName        = 1 + ModuleDefn
  final val FreshName       = 1 + NoneName
  final val LocalName       = 1 + FreshName
  final val PrimName        = 1 + LocalName
  final val ForeignName     = 1 + PrimName
  final val NestedName      = 1 + ForeignName
  final val ClassName       = 1 + NestedName
  final val ModuleName      = 1 + ClassName
  final val InterfaceName   = 1 + ModuleName
  final val FieldName       = 1 + InterfaceName
  final val ConstructorName = 1 + FieldName
  final val MethodName      = 1 + ConstructorName
  final val ArrayName       = 1 + MethodName
  final val TaggedName      = 1 + ArrayName

  final val UndefinedOp    = 1 + TaggedName
  final val RetOp          = 1 + UndefinedOp
  final val ThrowOp        = 1 + RetOp
  final val JumpOp         = 1 + ThrowOp
  final val IfOp           = 1 + JumpOp
  final val SwitchOp       = 1 + IfOp
  final val InvokeOp       = 1 + SwitchOp
  final val CallOp         = 1 + InvokeOp
  final val LoadOp         = 1 + CallOp
  final val StoreOp        = 1 + LoadOp
  final val ElemOp         = 1 + StoreOp
  final val ExtractOp      = 1 + ElemOp
  final val InsertOp       = 1 + ExtractOp
  final val AllocOp        = 1 + InsertOp
  final val AllocaOp       = 1 + AllocOp
  final val SizeOp         = 1 + AllocaOp
  final val BinOp          = 1 + SizeOp
  final val CompOp         = 1 + BinOp
  final val ConvOp         = 1 + CompOp
  final val FieldElemOp    = 1 + ConvOp
  final val MethodElemOp   = 1 + FieldElemOp
  final val AllocClassOp   = 1 + MethodElemOp
  final val AllocArrayOp   = 1 + AllocClassOp
  final val EqualsOp       = 1 + AllocArrayOp
  final val HashCodeOp     = 1 + EqualsOp
  final val GetClassOp     = 1 + HashCodeOp
  final val ClassOfOp      = 1 + GetClassOp
  final val AsInstanceOfOp = 1 + ClassOfOp
  final val IsInstanceOfOp = 1 + AsInstanceOfOp
  final val ArrayLengthOp  = 1 + IsInstanceOfOp
  final val ArrayElemOp    = 1 + ArrayLengthOp
  final val BoxOp          = 1 + ArrayElemOp
  final val UnboxOp        = 1 + BoxOp
  final val MonitorEnterOp = 1 + UnboxOp
  final val MonitorExitOp  = 1 + MonitorEnterOp
  final val StringConcatOp = 1 + MonitorExitOp
  final val ToStringOp     = 1 + StringConcatOp
  final val FromStringOp   = 1 + ToStringOp

  final val NoneType           = 1 + FromStringOp
  final val VoidType           = 1 + NoneType
  final val SizeType           = 1 + VoidType
  final val BoolType           = 1 + SizeType
  final val I8Type             = 1 + BoolType
  final val I16Type            = 1 + I8Type
  final val I32Type            = 1 + I16Type
  final val I64Type            = 1 + I32Type
  final val F32Type            = 1 + I64Type
  final val F64Type            = 1 + F32Type
  final val ArrayType          = 1 + F64Type
  final val PtrType            = 1 + ArrayType
  final val FunctionType       = 1 + PtrType
  final val StructType         = 1 + FunctionType
  final val UnitType           = 1 + StructType
  final val NothingType        = 1 + UnitType
  final val NullClassType      = 1 + NothingType
  final val ObjectClassType    = 1 + NullClassType
  final val ClassClassType     = 1 + ObjectClassType
  final val StringClassType    = 1 + ClassClassType
  final val CharacterClassType = 1 + StringClassType
  final val BooleanClassType   = 1 + CharacterClassType
  final val ByteClassType      = 1 + BooleanClassType
  final val ShortClassType     = 1 + ByteClassType
  final val IntegerClassType   = 1 + ShortClassType
  final val LongClassType      = 1 + IntegerClassType
  final val FloatClassType     = 1 + LongClassType
  final val DoubleClassType    = 1 + FloatClassType
  final val ClassType          = 1 + DoubleClassType
  final val ArrayClassType     = 1 + ClassType

  final val NoneVal   = 1 + DoubleClassType
  final val TrueVal   = 1 + NoneVal
  final val FalseVal  = 1 + TrueVal
  final val ZeroVal   = 1 + FalseVal
  final val I8Val     = 1 + ZeroVal
  final val I16Val    = 1 + I8Val
  final val I32Val    = 1 + I16Val
  final val I64Val    = 1 + I32Val
  final val F32Val    = 1 + I64Val
  final val F64Val    = 1 + F32Val
  final val StructVal = 1 + F64Val
  final val ArrayVal  = 1 + StructVal
  final val NameVal   = 1 + ArrayVal
  final val UnitVal   = 1 + NameVal
  final val NullVal   = 1 + UnitVal
  final val StringVal = 1 + NullVal
  final val ClassVal  = 1 + StringVal
}
