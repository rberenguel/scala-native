// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 1)
package scala.scalanative
package runtime

// Note 1:
// Arrays.scala is currently implemented using textual templating that is
// expanded through project/gyb.py script. After every update Arrays.scala.gyb
// one has to re-generate the source via:
//
// $ ./project/gyb.py \
//     nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb > \
//     nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala

// Note 2:
// Arrays of primitive types don't contain pointers, runtime.allocAtomic() is
// called for memory allocation. Array of objects do contain pointers.
// runtime.alloc() is called for memory allocation.

import scalanative.native._
import scalanative.runtime.Intrinsics._
import scala.annotation.unchecked.uncheckedStable

sealed abstract class Array[T]
    extends java.io.Serializable
    with java.lang.Cloneable {
  import Array._

  /** Number of elements of the array. */
  @inline def length: Int =
    this.cast[Ptr[Array.Layout]].length

  /** Size between elements in the array. */
  def stride: CSize

  /** Pointer to the element. */
  def at(i: Int): Ptr[T]

  /** Loads element at i, throws IndexOutOfBoundsException. */
  def apply(i: Int): T

  /** Stores value to element i, throws IndexOutOfBoundsException. */
  def update(i: Int, value: T): Unit

  /** Create a shallow of given array. */
  protected override def clone(): Array[T] =
    ??? // overriden in concrete classes
}

object Array {
  type Layout = (Ptr[Info], Int)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info] = !ptr._1
    def length: Int     = !ptr._2
  }

  def copy(from: AnyRef,
           fromPos: Int,
           to: AnyRef,
           toPos: Int,
           len: Int): Unit = {
    if (from == null || to == null) {
      throw new NullPointerException()
    } else if (!from.isInstanceOf[Array[_]]) {
      throw new IllegalArgumentException("from argument must be an array")
    } else if (!to.isInstanceOf[Array[_]]) {
      throw new IllegalArgumentException("to argument must be an array")
    } else {
      copy(from.asInstanceOf[Array[_]],
           fromPos,
           to.asInstanceOf[Array[_]],
           toPos,
           len)
    }
  }

  def copy(from: Array[_],
           fromPos: Int,
           to: Array[_],
           toPos: Int,
           len: Int): Unit = {
    if (from == null || to == null) {
      throw new NullPointerException()
    } else if (getInfo(from) != getInfo(to)) {
      throw new ArrayStoreException("Invalid array copy.")
    } else if (len < 0) {
      throw new IndexOutOfBoundsException("length is negative")
    } else if (fromPos < 0 || fromPos + len > from.length) {
      throw new IndexOutOfBoundsException(fromPos.toString)
    } else if (toPos < 0 || toPos + len > to.length) {
      throw new IndexOutOfBoundsException(toPos.toString)
    } else if (len == 0) {
      ()
    } else {
      val fromPtr = from.at(fromPos).cast[Ptr[Byte]]
      val toPtr   = to.at(toPos).cast[Ptr[Byte]]

      `llvm.memmove.p0i8.p0i8.i64`(toPtr, fromPtr, to.stride * len, 1, false)
    }
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class BooleanArray private () extends Array[Boolean] {
  import BooleanArray._

  @inline def stride: CSize =
    sizeof[Boolean]

  @inline def at(i: Int): Ptr[Boolean] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Boolean = !at(i)

  @inline def update(i: Int, value: Boolean): Unit = !at(i) = value

  @inline protected override def clone(): BooleanArray = {
    val size = sizeof[Array.Layout] + sizeof[Boolean] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[BooleanArray]
  }
}

object BooleanArray {
  type Layout = (Ptr[Info], Int, Boolean)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Boolean]         = ptr._3
  }

  @inline def alloc(length: Int): BooleanArray = {
    val size = sizeof[Array.Layout] + sizeof[Boolean] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[BooleanArray]
    arr.length = length
    arr.cast[BooleanArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class CharArray private () extends Array[Char] {
  import CharArray._

  @inline def stride: CSize =
    sizeof[Char]

  @inline def at(i: Int): Ptr[Char] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Char = !at(i)

  @inline def update(i: Int, value: Char): Unit = !at(i) = value

  @inline protected override def clone(): CharArray = {
    val size = sizeof[Array.Layout] + sizeof[Char] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[CharArray]
  }
}

object CharArray {
  type Layout = (Ptr[Info], Int, Char)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Char]            = ptr._3
  }

  @inline def alloc(length: Int): CharArray = {
    val size = sizeof[Array.Layout] + sizeof[Char] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[CharArray]
    arr.length = length
    arr.cast[CharArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class ByteArray private () extends Array[Byte] {
  import ByteArray._

  @inline def stride: CSize =
    sizeof[Byte]

  @inline def at(i: Int): Ptr[Byte] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Byte = !at(i)

  @inline def update(i: Int, value: Byte): Unit = !at(i) = value

  @inline protected override def clone(): ByteArray = {
    val size = sizeof[Array.Layout] + sizeof[Byte] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[ByteArray]
  }
}

object ByteArray {
  type Layout = (Ptr[Info], Int, Byte)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Byte]            = ptr._3
  }

  @inline def alloc(length: Int): ByteArray = {
    val size = sizeof[Array.Layout] + sizeof[Byte] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[ByteArray]
    arr.length = length
    arr.cast[ByteArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class ShortArray private () extends Array[Short] {
  import ShortArray._

  @inline def stride: CSize =
    sizeof[Short]

  @inline def at(i: Int): Ptr[Short] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Short = !at(i)

  @inline def update(i: Int, value: Short): Unit = !at(i) = value

  @inline protected override def clone(): ShortArray = {
    val size = sizeof[Array.Layout] + sizeof[Short] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[ShortArray]
  }
}

object ShortArray {
  type Layout = (Ptr[Info], Int, Short)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Short]           = ptr._3
  }

  @inline def alloc(length: Int): ShortArray = {
    val size = sizeof[Array.Layout] + sizeof[Short] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[ShortArray]
    arr.length = length
    arr.cast[ShortArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class IntArray private () extends Array[Int] {
  import IntArray._

  @inline def stride: CSize =
    sizeof[Int]

  @inline def at(i: Int): Ptr[Int] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Int = !at(i)

  @inline def update(i: Int, value: Int): Unit = !at(i) = value

  @inline protected override def clone(): IntArray = {
    val size = sizeof[Array.Layout] + sizeof[Int] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[IntArray]
  }
}

object IntArray {
  type Layout = (Ptr[Info], Int, Int)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Int]             = ptr._3
  }

  @inline def alloc(length: Int): IntArray = {
    val size = sizeof[Array.Layout] + sizeof[Int] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[IntArray]
    arr.length = length
    arr.cast[IntArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class LongArray private () extends Array[Long] {
  import LongArray._

  @inline def stride: CSize =
    sizeof[Long]

  @inline def at(i: Int): Ptr[Long] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Long = !at(i)

  @inline def update(i: Int, value: Long): Unit = !at(i) = value

  @inline protected override def clone(): LongArray = {
    val size = sizeof[Array.Layout] + sizeof[Long] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[LongArray]
  }
}

object LongArray {
  type Layout = (Ptr[Info], Int, Long)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Long]            = ptr._3
  }

  @inline def alloc(length: Int): LongArray = {
    val size = sizeof[Array.Layout] + sizeof[Long] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[LongArray]
    arr.length = length
    arr.cast[LongArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class FloatArray private () extends Array[Float] {
  import FloatArray._

  @inline def stride: CSize =
    sizeof[Float]

  @inline def at(i: Int): Ptr[Float] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Float = !at(i)

  @inline def update(i: Int, value: Float): Unit = !at(i) = value

  @inline protected override def clone(): FloatArray = {
    val size = sizeof[Array.Layout] + sizeof[Float] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[FloatArray]
  }
}

object FloatArray {
  type Layout = (Ptr[Info], Int, Float)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Float]           = ptr._3
  }

  @inline def alloc(length: Int): FloatArray = {
    val size = sizeof[Array.Layout] + sizeof[Float] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[FloatArray]
    arr.length = length
    arr.cast[FloatArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class DoubleArray private () extends Array[Double] {
  import DoubleArray._

  @inline def stride: CSize =
    sizeof[Double]

  @inline def at(i: Int): Ptr[Double] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Double = !at(i)

  @inline def update(i: Int, value: Double): Unit = !at(i) = value

  @inline protected override def clone(): DoubleArray = {
    val size = sizeof[Array.Layout] + sizeof[Double] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 118)
    val arr = GC.malloc_atomic(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[DoubleArray]
  }
}

object DoubleArray {
  type Layout = (Ptr[Info], Int, Double)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Double]          = ptr._3
  }

  @inline def alloc(length: Int): DoubleArray = {
    val size = sizeof[Array.Layout] + sizeof[Double] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 142)
      val ptr = GC.malloc_atomic(size)
      `llvm.memset.p0i8.i64`(ptr, 0, size, 1, false)
      ptr
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[DoubleArray]
    arr.length = length
    arr.cast[DoubleArray]
  }
}

// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 97)

final class ObjectArray private () extends Array[Object] {
  import ObjectArray._

  @inline def stride: CSize =
    sizeof[Object]

  @inline def at(i: Int): Ptr[Object] =
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException(i.toString)
    else {
      this.cast[Ptr[Layout]].data + i
    }

  @inline def apply(i: Int): Object = !at(i)

  @inline def update(i: Int, value: Object): Unit = !at(i) = value

  @inline protected override def clone(): ObjectArray = {
    val size = sizeof[Array.Layout] + sizeof[Object] * length
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 120)
    val arr = GC.malloc(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 122)
    `llvm.memcpy.p0i8.p0i8.i64`(arr, this.cast[Ptr[Byte]], size, 1, false)
    arr.cast[ObjectArray]
  }
}

object ObjectArray {
  type Layout = (Ptr[Info], Int, Object)

  implicit class LayoutOps(val ptr: Ptr[Layout]) extends AnyVal {
    def info: Ptr[Info]            = !ptr._1
    def info_=(v: Ptr[Info]): Unit = !ptr._1 = v
    def length: Int                = !ptr._2
    def length_=(v: Int): Unit     = !ptr._2 = v
    def data: Ptr[Object]          = ptr._3
  }

  @inline def alloc(length: Int): ObjectArray = {
    val size = sizeof[Array.Layout] + sizeof[Object] * length
    val arr = {
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 146)
      val arr = GC.malloc(size)
// ###sourceLocation(file: "/Users/Denys/.src/native/nativelib/src/main/scala/scala/scalanative/runtime/Arrays.scala.gyb", line: 148)
    }.cast[Ptr[Layout]]
    arr.info = infoof[ObjectArray]
    arr.length = length
    arr.cast[ObjectArray]
  }
}
