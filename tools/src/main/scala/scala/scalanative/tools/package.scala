package scala.scalanative

import java.nio.file.Paths
import scalanative.io.withScratchBuffer
import scalanative.nir.Global

// API use-cases
//
// sbt plugin:
//   1. link & compile code
//
// compiler  test : VirtualDirectory -> World
//   1. virtual file system of some kind
//   2. instantiate scalac+nscplugin
//   3. run it and check invariants
//
// optimizer test : World -> World
//   1. define what passes we want
//   2. run them and check invariants
//
// code gen test : World -> Seq[String]
//   1. run code gen and check the string

package object tools {
  val World = linker.World

  type LinkerPath = linker.Path
  val LinkerPath = linker.Path

  type LinkerReporter = linker.Reporter
  val LinkerReporter = linker.Reporter

  type OptimizerDriver = optimizer.Driver
  val OptimizerDriver = optimizer.Driver

  type OptimizerReporter = optimizer.Reporter
  val OptimizerReporter = optimizer.Reporter

  /** Given the classpath and entry point, link under closed-world assumption. */
  def link(config: Config,
           driver: OptimizerDriver,
           reporter: LinkerReporter): World.Top = {
    val paths     = config.paths
    val injectors = linker.Linker.injects ++ driver.passes
    val depends   = driver.passes
    val entries =
      config.main.toSeq.map { main =>
        nir.Global.Member(main, "main_class.ssnr.ObjectArray_unit")
      }

    linker.Linker(config, injectors, depends, reporter).link(entries)
  }

  /** Link without main entry point. */
  def link(paths: Seq[LinkerPath], entries: Seq[Global]): World.Top = {
    val config = Config.empty.withPaths(paths)
    linker.Linker(config, Seq(), Seq(), LinkerReporter.empty).link(entries)
  }

  /** Transform high-level closed world to its lower-level counterpart. */
  def optimize(config: Config,
               driver: OptimizerDriver,
               world: World.Top,
               reporter: OptimizerReporter = OptimizerReporter.empty): Unit =
    optimizer.Optimizer(config, driver, world, reporter)

  /** Given low-level assembly, emit LLVM IR for it to the buildDirectory. */
  def codegen(config: Config, world: World.Top): Unit = {
    val gen = scalanative.codegen.CodeGen(world)

    withScratchBuffer { buffer =>
      gen.gen(buffer)
      buffer.flip
      config.targetDirectory.write(Paths.get("out.ll"), buffer)
    }
  }
}
