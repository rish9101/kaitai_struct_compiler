package io.kaitai.struct.format

import io.kaitai.struct.precompile.ErrorInInput

import scala.collection.mutable
import scala.concurrent.Future

/**
  * Top-level abstract container for all ClassSpecs. Used for recursive
  * loading of imports. Real-life implementation depend on file handling
  * (which at least differ between JVM vs JS), and thus implementations
  * are platform-dependent.
  */
abstract class ProtocolSpecs(val firstSpec: ProtocolSpec) extends mutable.HashMap[String, ProtocolSpec] {
  this(firstSpec.name.head) = firstSpec

  /**
    * Calls certain function on all [[StructSpec]] elements stored in this ClassSpecs,
    * and all subtypes stored in these elements, recursively.
    */
  def forEachRec(proc: (ClassSpec) => Unit): Unit =
    forEachTopLevel((_, typeSpec) => typeSpec.forEachRec(proc))

  /**
    * Calls certain function on all top-level [[StructSpec]] elements stored in this
    * ClassSpecs.
    */
  def forEachTopLevel(proc: (String, ProtocolSpec) => Unit): Unit = {
    foreach { case (specName, typeSpec) =>
      try {
        proc(specName, typeSpec)
      } catch {
        case ErrorInInput(err, path, None) =>
          // Try to emit more specific error, with a reference to current file
          throw ErrorInInput(err, path, Some(specName))
      }
    }
  }

  def importRelative(name: String, path: List[String], inFile: Option[String]): Future[Option[ProtocolSpec]]
  def importAbsolute(name: String, path: List[String], inFile: Option[String]): Future[Option[ProtocolSpec]]
}
