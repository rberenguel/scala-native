// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 1)
package scala.scalanative
package native

import scalanative.runtime.{undefined, Tag}

/** C-style function pointer. */
sealed abstract class FunctionPtr

object FunctionPtr {

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction0[R](f: Function0[R]): FunctionPtr0[R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction1[T1, R](f: Function1[T1, R]): FunctionPtr1[T1, R] =
    undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction2[T1, T2, R](
      f: Function2[T1, T2, R]): FunctionPtr2[T1, T2, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction3[T1, T2, T3, R](
      f: Function3[T1, T2, T3, R]): FunctionPtr3[T1, T2, T3, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction4[T1, T2, T3, T4, R](
      f: Function4[T1, T2, T3, T4, R]): FunctionPtr4[T1, T2, T3, T4, R] =
    undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction5[T1, T2, T3, T4, T5, R](
      f: Function5[T1, T2, T3, T4, T5, R])
    : FunctionPtr5[T1, T2, T3, T4, T5, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction6[T1, T2, T3, T4, T5, T6, R](
      f: Function6[T1, T2, T3, T4, T5, T6, R])
    : FunctionPtr6[T1, T2, T3, T4, T5, T6, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction7[T1, T2, T3, T4, T5, T6, T7, R](
      f: Function7[T1, T2, T3, T4, T5, T6, T7, R])
    : FunctionPtr7[T1, T2, T3, T4, T5, T6, T7, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](
      f: Function8[T1, T2, T3, T4, T5, T6, T7, T8, R])
    : FunctionPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](
      f: Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R])
    : FunctionPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](
      f: Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R])
    : FunctionPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](
      f: Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R])
    : FunctionPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] =
    undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction12[T1,
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
                              R](
      f: Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R])
    : FunctionPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] =
    undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction13[T1,
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
                              R](f: Function13[T1,
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
                                               R]): FunctionPtr13[T1,
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
                                                                  R] =
    undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction14[T1,
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
                              R](
      f: Function14[T1,
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
                    R]): FunctionPtr14[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction15[T1,
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
                              R](
      f: Function15[T1,
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
                    R]): FunctionPtr15[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction16[T1,
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
                              R](
      f: Function16[T1,
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
                    R]): FunctionPtr16[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction17[T1,
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
                              R](
      f: Function17[T1,
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
                    R]): FunctionPtr17[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction18[T1,
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
                              R](
      f: Function18[T1,
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
                    R]): FunctionPtr18[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction19[T1,
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
                              R](
      f: Function19[T1,
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
                    R]): FunctionPtr19[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction20[T1,
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
                              R](
      f: Function20[T1,
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
                    R]): FunctionPtr20[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction21[T1,
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
                              R](
      f: Function21[T1,
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
                    R]): FunctionPtr21[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 13)

  implicit def fromFunction22[T1,
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
                              R](
      f: Function22[T1,
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
                    R]): FunctionPtr22[T1,
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
                                       R] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 17)

}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr0[R] extends FunctionPtr {
  def apply()(implicit R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr1[T1, R] extends FunctionPtr {
  def apply(arg1: T1)(implicit T1: Tag[T1], R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr2[T1, T2, R] extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2)(implicit T1: Tag[T1], T2: Tag[T2], R: Tag[R]): R =
    undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr3[T1, T2, T3, R] extends FunctionPtr {
  def apply(arg1: T1, arg2: T2, arg3: T3)(implicit T1: Tag[T1],
                                          T2: Tag[T2],
                                          T3: Tag[T3],
                                          R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr4[T1, T2, T3, T4, R] extends FunctionPtr {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4)(implicit T1: Tag[T1],
                                                    T2: Tag[T2],
                                                    T3: Tag[T3],
                                                    T4: Tag[T4],
                                                    R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr5[T1, T2, T3, T4, T5, R] extends FunctionPtr {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5)(
      implicit T1: Tag[T1],
      T2: Tag[T2],
      T3: Tag[T3],
      T4: Tag[T4],
      T5: Tag[T5],
      R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr6[T1, T2, T3, T4, T5, T6, R] extends FunctionPtr {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6)(
      implicit T1: Tag[T1],
      T2: Tag[T2],
      T3: Tag[T3],
      T4: Tag[T4],
      T5: Tag[T5],
      T6: Tag[T6],
      R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr7[T1, T2, T3, T4, T5, T6, T7, R] extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7)(implicit T1: Tag[T1],
                      T2: Tag[T2],
                      T3: Tag[T3],
                      T4: Tag[T4],
                      T5: Tag[T5],
                      T6: Tag[T6],
                      T7: Tag[T7],
                      R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr8[T1, T2, T3, T4, T5, T6, T7, T8, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8)(implicit T1: Tag[T1],
                      T2: Tag[T2],
                      T3: Tag[T3],
                      T4: Tag[T4],
                      T5: Tag[T5],
                      T6: Tag[T6],
                      T7: Tag[T7],
                      T8: Tag[T8],
                      R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9)(implicit T1: Tag[T1],
                      T2: Tag[T2],
                      T3: Tag[T3],
                      T4: Tag[T4],
                      T5: Tag[T5],
                      T6: Tag[T6],
                      T7: Tag[T7],
                      T8: Tag[T8],
                      T9: Tag[T9],
                      R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr13[
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr14[
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr15[
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr16[
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr17[T1,
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
                          R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16,
            arg17: T17)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        T17: Tag[T17],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr18[T1,
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
                          R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16,
            arg17: T17,
            arg18: T18)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        T17: Tag[T17],
                        T18: Tag[T18],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr19[T1,
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
                          R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16,
            arg17: T17,
            arg18: T18,
            arg19: T19)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        T17: Tag[T17],
                        T18: Tag[T18],
                        T19: Tag[T19],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr20[T1,
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
                          R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16,
            arg17: T17,
            arg18: T18,
            arg19: T19,
            arg20: T20)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        T17: Tag[T17],
                        T18: Tag[T18],
                        T19: Tag[T19],
                        T20: Tag[T20],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr21[T1,
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
                          R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16,
            arg17: T17,
            arg18: T18,
            arg19: T19,
            arg20: T20,
            arg21: T21)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        T17: Tag[T17],
                        T18: Tag[T18],
                        T19: Tag[T19],
                        T20: Tag[T20],
                        T21: Tag[T21],
                        R: Tag[R]): R = undefined
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/FunctionPtr.scala.gyb", line: 24)

final class FunctionPtr22[T1,
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
                          R]
    extends FunctionPtr {
  def apply(arg1: T1,
            arg2: T2,
            arg3: T3,
            arg4: T4,
            arg5: T5,
            arg6: T6,
            arg7: T7,
            arg8: T8,
            arg9: T9,
            arg10: T10,
            arg11: T11,
            arg12: T12,
            arg13: T13,
            arg14: T14,
            arg15: T15,
            arg16: T16,
            arg17: T17,
            arg18: T18,
            arg19: T19,
            arg20: T20,
            arg21: T21,
            arg22: T22)(implicit T1: Tag[T1],
                        T2: Tag[T2],
                        T3: Tag[T3],
                        T4: Tag[T4],
                        T5: Tag[T5],
                        T6: Tag[T6],
                        T7: Tag[T7],
                        T8: Tag[T8],
                        T9: Tag[T9],
                        T10: Tag[T10],
                        T11: Tag[T11],
                        T12: Tag[T12],
                        T13: Tag[T13],
                        T14: Tag[T14],
                        T15: Tag[T15],
                        T16: Tag[T16],
                        T17: Tag[T17],
                        T18: Tag[T18],
                        T19: Tag[T19],
                        T20: Tag[T20],
                        T21: Tag[T21],
                        T22: Tag[T22],
                        R: Tag[R]): R = undefined
}
