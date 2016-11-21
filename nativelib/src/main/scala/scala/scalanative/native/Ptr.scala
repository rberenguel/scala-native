// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 1)
package scala.scalanative
package native

import scala.language.dynamics
import scalanative.runtime._

/** The C `const T *` pointer. */
final class Ptr[T] private () {

  /** Dereference a pointer. */
  def unary_!(implicit T: Tag[T]): T = undefined

  /** Store a value to the address pointed at by a pointer. */
  def `unary_!_=`(value: T)(implicit T: Tag[T]): Unit = undefined

  /** Compute a derived pointer by adding given offset. */
  def +(offset: Word)(implicit T: Tag[T]): Ptr[T] = undefined

  /** Compute a derived pointer by substritaging given offset. */
  def -(offset: Word)(implicit T: Tag[T]): Ptr[T] = undefined

  /** Read a value at given offset. Equivalent to !(offset + word). */
  def apply(offset: Word)(implicit T: Tag[T]): T = undefined

  /** Store a value to given offset. Equivalent to !(offset + word) = value. */
  def update(offset: Word, value: T)(implicit T: Tag[T]): T = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 1-th member of a produtag. */
  def _1[F](implicit T: Tag[T], F: Field1[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 2-th member of a produtag. */
  def _2[F](implicit T: Tag[T], F: Field2[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 3-th member of a produtag. */
  def _3[F](implicit T: Tag[T], F: Field3[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 4-th member of a produtag. */
  def _4[F](implicit T: Tag[T], F: Field4[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 5-th member of a produtag. */
  def _5[F](implicit T: Tag[T], F: Field5[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 6-th member of a produtag. */
  def _6[F](implicit T: Tag[T], F: Field6[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 7-th member of a produtag. */
  def _7[F](implicit T: Tag[T], F: Field7[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 8-th member of a produtag. */
  def _8[F](implicit T: Tag[T], F: Field8[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 9-th member of a produtag. */
  def _9[F](implicit T: Tag[T], F: Field9[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 10-th member of a produtag. */
  def _10[F](implicit T: Tag[T], F: Field10[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 11-th member of a produtag. */
  def _11[F](implicit T: Tag[T], F: Field11[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 12-th member of a produtag. */
  def _12[F](implicit T: Tag[T], F: Field12[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 13-th member of a produtag. */
  def _13[F](implicit T: Tag[T], F: Field13[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 14-th member of a produtag. */
  def _14[F](implicit T: Tag[T], F: Field14[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 15-th member of a produtag. */
  def _15[F](implicit T: Tag[T], F: Field15[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 16-th member of a produtag. */
  def _16[F](implicit T: Tag[T], F: Field16[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 17-th member of a produtag. */
  def _17[F](implicit T: Tag[T], F: Field17[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 18-th member of a produtag. */
  def _18[F](implicit T: Tag[T], F: Field18[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 19-th member of a produtag. */
  def _19[F](implicit T: Tag[T], F: Field19[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 20-th member of a produtag. */
  def _20[F](implicit T: Tag[T], F: Field20[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 21-th member of a produtag. */
  def _21[F](implicit T: Tag[T], F: Field21[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 29)

  /** Get a derived pointer to the 22-th member of a produtag. */
  def _22[F](implicit T: Tag[T], F: Field22[T, F]): Ptr[F] = undefined

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/native/Ptr.scala.gyb", line: 34)
}
