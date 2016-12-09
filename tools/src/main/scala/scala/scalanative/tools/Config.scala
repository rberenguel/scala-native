package scala.scalanative
package tools

import scalanative.io.VirtualDirectory
import nir.Global

sealed trait Config {

  /** Main point for linking. */
  def main: Option[Global]

  /** Sequence of all NIR locations. */
  def paths: Seq[LinkerPath]

  /** Directory to emit intermediate compilation results. */
  def targetDirectory: VirtualDirectory

  /** Create new config with given main point. */
  def withMain(value: Option[Global]): Config

  /** Create a new config with given nir paths. */
  def withPaths(value: Seq[LinkerPath]): Config

  /** Create a new config with given directory. */
  def withTargetDirectory(value: VirtualDirectory): Config
}

object Config {

  /** Default empty config object. */
  val empty: Config =
    Impl(main = None,
         paths = Seq.empty,
         targetDirectory = VirtualDirectory.empty)

  private final case class Impl(main: Option[Global],
                                paths: Seq[LinkerPath],
                                targetDirectory: VirtualDirectory)
      extends Config {
    def withMain(value: Option[Global]): Config =
      copy(main = value)

    def withPaths(value: Seq[LinkerPath]): Config =
      copy(paths = value)

    def withTargetDirectory(value: VirtualDirectory): Config =
      copy(targetDirectory = value)
  }
}
