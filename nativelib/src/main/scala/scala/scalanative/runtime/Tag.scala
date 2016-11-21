// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 1)
package scala.scalanative
package runtime

import scala.reflect.ClassTag
import native._

sealed trait Tag[P]

object Tag {
  implicit val Unit: Tag[Unit]       = new Tag[Unit]    {}
  implicit val Boolean: Tag[Boolean] = new Tag[Boolean] {}
  implicit val Char: Tag[Char]       = new Tag[Char]    {}
  implicit val Byte: Tag[Byte]       = new Tag[Byte]    {}
  implicit val UByte: Tag[UByte]     = new Tag[UByte]   {}
  implicit val Short: Tag[Short]     = new Tag[Short]   {}
  implicit val UShort: Tag[UShort]   = new Tag[UShort]  {}
  implicit val Int: Tag[Int]         = new Tag[Int]     {}
  implicit val UInt: Tag[UInt]       = new Tag[UInt]    {}
  implicit val Long: Tag[Long]       = new Tag[Long]    {}
  implicit val ULong: Tag[ULong]     = new Tag[ULong]   {}
  implicit val Float: Tag[Float]     = new Tag[Float]   {}
  implicit val Double: Tag[Double]   = new Tag[Double]  {}

  implicit def Ptr[T: Tag]: Tag[Ptr[T]] = new Tag[Ptr[T]] {}

  implicit def Class[T <: AnyRef: ClassTag]: Tag[T] = new Tag[T] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple1[T1: Tag]: Tag[Tuple1[T1]] =
    new Tag[Tuple1[T1]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple2[T1: Tag, T2: Tag]: Tag[Tuple2[T1, T2]] =
    new Tag[Tuple2[T1, T2]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple3[T1: Tag, T2: Tag, T3: Tag]: Tag[Tuple3[T1, T2, T3]] =
    new Tag[Tuple3[T1, T2, T3]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple4[T1: Tag, T2: Tag, T3: Tag, T4: Tag]
    : Tag[Tuple4[T1, T2, T3, T4]] =
    new Tag[Tuple4[T1, T2, T3, T4]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple5[T1: Tag, T2: Tag, T3: Tag, T4: Tag, T5: Tag]
    : Tag[Tuple5[T1, T2, T3, T4, T5]] =
    new Tag[Tuple5[T1, T2, T3, T4, T5]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple6[T1: Tag, T2: Tag, T3: Tag, T4: Tag, T5: Tag, T6: Tag]
    : Tag[Tuple6[T1, T2, T3, T4, T5, T6]] =
    new Tag[Tuple6[T1, T2, T3, T4, T5, T6]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple7[T1: Tag,
                      T2: Tag,
                      T3: Tag,
                      T4: Tag,
                      T5: Tag,
                      T6: Tag,
                      T7: Tag]: Tag[Tuple7[T1, T2, T3, T4, T5, T6, T7]] =
    new Tag[Tuple7[T1, T2, T3, T4, T5, T6, T7]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple8[T1: Tag,
                      T2: Tag,
                      T3: Tag,
                      T4: Tag,
                      T5: Tag,
                      T6: Tag,
                      T7: Tag,
                      T8: Tag]: Tag[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] =
    new Tag[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple9[T1: Tag,
                      T2: Tag,
                      T3: Tag,
                      T4: Tag,
                      T5: Tag,
                      T6: Tag,
                      T7: Tag,
                      T8: Tag,
                      T9: Tag]
    : Tag[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] =
    new Tag[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple10[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag]
    : Tag[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
    new Tag[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple11[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag]
    : Tag[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] =
    new Tag[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple12[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag]
    : Tag[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] =
    new Tag[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple13[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag]
    : Tag[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] =
    new Tag[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple14[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag]: Tag[
    Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] =
    new Tag[
      Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple15[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag]
    : Tag[
      Tuple15[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15]] =
    new Tag[
      Tuple15[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple16[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag]: Tag[
    Tuple16[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16]] =
    new Tag[
      Tuple16[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple17[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag,
                       T17: Tag]: Tag[
    Tuple17[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17]] =
    new Tag[
      Tuple17[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16,
              T17]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple18[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag,
                       T17: Tag,
                       T18: Tag]: Tag[
    Tuple18[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18]] =
    new Tag[
      Tuple18[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16,
              T17,
              T18]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple19[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag,
                       T17: Tag,
                       T18: Tag,
                       T19: Tag]: Tag[
    Tuple19[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19]] =
    new Tag[
      Tuple19[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16,
              T17,
              T18,
              T19]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple20[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag,
                       T17: Tag,
                       T18: Tag,
                       T19: Tag,
                       T20: Tag]: Tag[
    Tuple20[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20]] =
    new Tag[
      Tuple20[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16,
              T17,
              T18,
              T19,
              T20]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple21[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag,
                       T17: Tag,
                       T18: Tag,
                       T19: Tag,
                       T20: Tag,
                       T21: Tag]: Tag[
    Tuple21[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21]] =
    new Tag[
      Tuple21[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16,
              T17,
              T18,
              T19,
              T20,
              T21]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 32)

  implicit def Tuple22[T1: Tag,
                       T2: Tag,
                       T3: Tag,
                       T4: Tag,
                       T5: Tag,
                       T6: Tag,
                       T7: Tag,
                       T8: Tag,
                       T9: Tag,
                       T10: Tag,
                       T11: Tag,
                       T12: Tag,
                       T13: Tag,
                       T14: Tag,
                       T15: Tag,
                       T16: Tag,
                       T17: Tag,
                       T18: Tag,
                       T19: Tag,
                       T20: Tag,
                       T21: Tag,
                       T22: Tag]: Tag[
    Tuple22[T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
            T16,
            T17,
            T18,
            T19,
            T20,
            T21,
            T22]] =
    new Tag[
      Tuple22[T1,
              T2,
              T3,
              T4,
              T5,
              T6,
              T7,
              T8,
              T9,
              T10,
              T11,
              T12,
              T13,
              T14,
              T15,
              T16,
              T17,
              T18,
              T19,
              T20,
              T21,
              T22]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 37)

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr0[R: Tag]: Tag[FunctionPtr0[R]] =
    new Tag[FunctionPtr0[R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr1[T1: Tag, R: Tag]: Tag[FunctionPtr1[T1, R]] =
    new Tag[FunctionPtr1[T1, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr2[T1: Tag, T2: Tag, R: Tag]
    : Tag[FunctionPtr2[T1, T2, R]] =
    new Tag[FunctionPtr2[T1, T2, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr3[T1: Tag, T2: Tag, T3: Tag, R: Tag]
    : Tag[FunctionPtr3[T1, T2, T3, R]] =
    new Tag[FunctionPtr3[T1, T2, T3, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr4[T1: Tag, T2: Tag, T3: Tag, T4: Tag, R: Tag]
    : Tag[FunctionPtr4[T1, T2, T3, T4, R]] =
    new Tag[FunctionPtr4[T1, T2, T3, T4, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr5[T1: Tag,
                            T2: Tag,
                            T3: Tag,
                            T4: Tag,
                            T5: Tag,
                            R: Tag]: Tag[FunctionPtr5[T1, T2, T3, T4, T5, R]] =
    new Tag[FunctionPtr5[T1, T2, T3, T4, T5, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr6[T1: Tag,
                            T2: Tag,
                            T3: Tag,
                            T4: Tag,
                            T5: Tag,
                            T6: Tag,
                            R: Tag]
    : Tag[FunctionPtr6[T1, T2, T3, T4, T5, T6, R]] =
    new Tag[FunctionPtr6[T1, T2, T3, T4, T5, T6, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr7[T1: Tag,
                            T2: Tag,
                            T3: Tag,
                            T4: Tag,
                            T5: Tag,
                            T6: Tag,
                            T7: Tag,
                            R: Tag]
    : Tag[FunctionPtr7[T1, T2, T3, T4, T5, T6, T7, R]] =
    new Tag[FunctionPtr7[T1, T2, T3, T4, T5, T6, T7, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr8[T1: Tag,
                            T2: Tag,
                            T3: Tag,
                            T4: Tag,
                            T5: Tag,
                            T6: Tag,
                            T7: Tag,
                            T8: Tag,
                            R: Tag]
    : Tag[FunctionPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R]] =
    new Tag[FunctionPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr9[T1: Tag,
                            T2: Tag,
                            T3: Tag,
                            T4: Tag,
                            T5: Tag,
                            T6: Tag,
                            T7: Tag,
                            T8: Tag,
                            T9: Tag,
                            R: Tag]
    : Tag[FunctionPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]] =
    new Tag[FunctionPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr10[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             R: Tag]
    : Tag[FunctionPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]] =
    new Tag[FunctionPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr11[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             R: Tag]
    : Tag[FunctionPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]] =
    new Tag[FunctionPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr12[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             R: Tag]: Tag[
    FunctionPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]] =
    new Tag[
      FunctionPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr13[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             R: Tag]: Tag[
    FunctionPtr13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]] =
    new Tag[FunctionPtr13[T1,
                          T2,
                          T3,
                          T4,
                          T5,
                          T6,
                          T7,
                          T8,
                          T9,
                          T10,
                          T11,
                          T12,
                          T13,
                          R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr14[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             R: Tag]: Tag[
    FunctionPtr14[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  R]] =
    new Tag[
      FunctionPtr14[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr15[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             R: Tag]: Tag[
    FunctionPtr15[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  R]] =
    new Tag[
      FunctionPtr15[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr16[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             R: Tag]: Tag[
    FunctionPtr16[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  R]] =
    new Tag[
      FunctionPtr16[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr17[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             T17: Tag,
                             R: Tag]: Tag[
    FunctionPtr17[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  T17,
                  R]] =
    new Tag[
      FunctionPtr17[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    T17,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr18[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             T17: Tag,
                             T18: Tag,
                             R: Tag]: Tag[
    FunctionPtr18[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  T17,
                  T18,
                  R]] =
    new Tag[
      FunctionPtr18[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    T17,
                    T18,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr19[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             T17: Tag,
                             T18: Tag,
                             T19: Tag,
                             R: Tag]: Tag[
    FunctionPtr19[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  T17,
                  T18,
                  T19,
                  R]] =
    new Tag[
      FunctionPtr19[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    T17,
                    T18,
                    T19,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr20[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             T17: Tag,
                             T18: Tag,
                             T19: Tag,
                             T20: Tag,
                             R: Tag]: Tag[
    FunctionPtr20[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  T17,
                  T18,
                  T19,
                  T20,
                  R]] =
    new Tag[
      FunctionPtr20[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    T17,
                    T18,
                    T19,
                    T20,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr21[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             T17: Tag,
                             T18: Tag,
                             T19: Tag,
                             T20: Tag,
                             T21: Tag,
                             R: Tag]: Tag[
    FunctionPtr21[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  T17,
                  T18,
                  T19,
                  T20,
                  T21,
                  R]] =
    new Tag[
      FunctionPtr21[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    T17,
                    T18,
                    T19,
                    T20,
                    T21,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 42)

  implicit def FunctionPtr22[T1: Tag,
                             T2: Tag,
                             T3: Tag,
                             T4: Tag,
                             T5: Tag,
                             T6: Tag,
                             T7: Tag,
                             T8: Tag,
                             T9: Tag,
                             T10: Tag,
                             T11: Tag,
                             T12: Tag,
                             T13: Tag,
                             T14: Tag,
                             T15: Tag,
                             T16: Tag,
                             T17: Tag,
                             T18: Tag,
                             T19: Tag,
                             T20: Tag,
                             T21: Tag,
                             T22: Tag,
                             R: Tag]: Tag[
    FunctionPtr22[T1,
                  T2,
                  T3,
                  T4,
                  T5,
                  T6,
                  T7,
                  T8,
                  T9,
                  T10,
                  T11,
                  T12,
                  T13,
                  T14,
                  T15,
                  T16,
                  T17,
                  T18,
                  T19,
                  T20,
                  T21,
                  T22,
                  R]] =
    new Tag[
      FunctionPtr22[T1,
                    T2,
                    T3,
                    T4,
                    T5,
                    T6,
                    T7,
                    T8,
                    T9,
                    T10,
                    T11,
                    T12,
                    T13,
                    T14,
                    T15,
                    T16,
                    T17,
                    T18,
                    T19,
                    T20,
                    T21,
                    T22,
                    R]] {}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Tag.scala.gyb", line: 47)
}
